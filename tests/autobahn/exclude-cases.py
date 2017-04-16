# Travis only allows 50-minute jobs so we unfortunately cannot run all test
# cases.  This script selects all the long cases based on a report from a
# previous test run.  These can then be added to the 'exclude-cases' field.
#
# There are also some inherently broken cases.  See:
#
# <http://autobahn.ws/reports/servers/>

import argparse
import json

BROKEN_CASES = [
    '12.4.5',
    '12.4.6',
    '12.4.11',
    '12.4.18',
    '12.4.13',
    '12.4.10',
    '12.4.17',
    '12.4.16',
    '12.4.15',
    '12.4.14',
    '12.4.9',
    '12.4.8',
    '12.5.5',
    '12.5.6',
    '12.5.8',
    '12.5.9',
    '12.5.10',
    '12.5.11',
    '12.5.13',
    '12.5.14',
    '12.5.15',
    '12.5.16',
    '12.5.17',
    '12.5.18'
]

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('report', help='JSON report', nargs='?')
    parser.add_argument('--duration', type=int, help='Duration treshold',
            default=1000)
    options = parser.parse_args()

    exclude_cases = []

    # Exclude long tests from report
    if options.report:
        with open(options.report) as f:
            report = json.load(f)

            for server in report:
                server_report = report[server]
                for case_name in server_report:
                    case_report = server_report[case_name]
                    if case_report['duration'] >= options.duration:
                        exclude_cases += [case_name]

    # Exclude broken tests
    for case_name in BROKEN_CASES:
        if not case_name in exclude_cases:
            exclude_cases += [case_name]

    print(json.dumps(exclude_cases))
