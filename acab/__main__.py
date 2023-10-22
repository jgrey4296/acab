#/usr/bin/env python3
"""
A Simple __main__ for running the acab repl via
python -m acab
"""
if __name__ == '__main__':
    import acab.cli.repl_main
    acab.cli.repl_main.main_repl()
