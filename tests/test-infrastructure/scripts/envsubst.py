#!/usr/bin/python3
"""
NAME
       envsubst.py - substitutes environment variables in bash format strings

DESCRIPTION
    envsubst.py is upgrade of POSIX command `envsubst`

    supported syntax:
      normal       - ${VARIABLE1}
      with default - ${VARIABLE1:-somevalue}
"""

import os
import re
import sys


def envsubst(template_str, env=os.environ):
    """Substitute environment variables in the template string, supporting default values."""
    pattern = re.compile(r'\$\{([^}:\s]+)(?::-(.*?))?\}')

    def replace(match):
        var = match.group(1)
        default_value = match.group(2) if match.group(2) is not None else ''
        return env.get(var, default_value)

    return pattern.sub(replace, template_str)


def main():
    if len(sys.argv) > 2:
        print("Usage: python envsubst.py [template_file]")
        sys.exit(1)

    if len(sys.argv) == 2:
        template_file = sys.argv[1]
        with open(template_file, 'r') as file:
            template_str = file.read()
    else:
        template_str = sys.stdin.read()

    result = envsubst(template_str)

    print(result)


if __name__ == "__main__":
    main()
