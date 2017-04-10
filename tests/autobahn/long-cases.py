# Travis only allows 50-minute jobs so we unfortunately cannot run all test
# cases.  This script selects all the long cases based on a report from a
# previous test run.  These can then be added to the 'exclude-cases' field.

import argparse
import json

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('report', help='JSON report')
    parser.add_argument('--duration', type=int, help='Duration treshold',
            default=2000)
    options = parser.parse_args()

    with open(options.report) as f:
        report = json.load(f)

    long_cases = []

    for server in report:
        server_report = report[server]
        for case_name in server_report:
            case_report = server_report[case_name]
            if case_report['duration'] >= options.duration:
                long_cases += [case_name]

    print(json.dumps(long_cases))
