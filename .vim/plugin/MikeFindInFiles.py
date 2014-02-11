try:
    import fnmatch
    import mmap
    import os
    import re
    import sys
    import vim

    from difflib import SequenceMatcher

    matches = []
    ignored_directories = [
        ".git",
        ".idea",
        "migrations",
        "ve",
    ]
    ignored_extensions = (
        ".swp",
        ".png",
        ".jpg",
        ".pyc",
    )

    search_string = None

    vim.command('call inputsave()')
    try:
        vim.command("let user_input = input('Find files matching string: ')")
        vim.command('call inputrestore()')
        search_string = vim.eval('user_input')
    except KeyboardInterrupt:
        # User cancelled input dialog.
        pass

    if search_string:
        # regex_string = ".*?".join([re.escape(char) for char in search_string])
        # regex_string = re.escape(search_string)
        the_regex = re.compile(search_string.lower())

        for root, dirnames, filenames in os.walk('.', topdown=True):
            for ignored_dir in ignored_directories:
                if ignored_dir in dirnames:
                    dirnames.remove(ignored_dir)

            for file_name in filenames:
                if file_name.endswith(ignored_extensions):
                    continue

                with open(file_name) as f:
                    current_line = 0
                    for line in f:
                        current_line += 1
                        if the_regex.search(line):

                    file_path = os.path.realpath(os.path.join(
                        os.getcwd(),
                        root,
                        file_name,
                    ))
                    matches.append(
                        (
                            file_name,
                            file_path.replace(
                                os.path.abspath("."),
                                ""
                            ).replace(
                                file_name,
                                ""
                            ),
                            file_path.replace(" ", "\\ "),
                        )
                    )

        to_return = []
        for match_tuple in matches:
            to_return.append((
                SequenceMatcher(
                    lambda x: x == " ",
                    search_string,
                    match_tuple[0],
                ).ratio(),
                match_tuple,
            ))

        to_return = [i[1][2] for i in sorted(to_return,
            key=lambda x: x[0],
            reverse=True
        )]

        vim.command("let l:toReturn = {0}".format(to_return))
    else:
        # User didn't enter any input.
        pass

except ImportError, e:
    print "Error while importing from Python within plugin: {}".format(e)
