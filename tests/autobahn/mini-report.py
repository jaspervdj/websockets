# `wstest` doesn't actually set an informational error code so we'll need to do
# it ourselves.

import argparse
import json
import sys

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('report', help='JSON report')
    options = parser.parse_args()

    with open(options.report) as f:
        report = json.load(f)

    behaviors = {}

    for server in report:
        server_report = report[server]
        for case_name in server_report:
            case_report = server_report[case_name]
            behavior = case_report['behavior']
            if behavior in behaviors:
                behaviors[behavior] += [case_name]
            else:
                behaviors[behavior] = [case_name]

    if 'FAILED' in behaviors:
        print(' Failed cases:')
        for case_name in behaviors['FAILED']:
            print('- ' + case_name)
        sys.exit(1)
    else:
        print(str(len(behaviors['OK'])) + ' cases passed')
