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

    # Get some input from the user.
    vim.command('call inputsave()')
    try:
        vim.command("let user_input = input('Search for string in files: ')")
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
                # Remove dirnames that we don't want to recurse on.
                if ignored_dir in dirnames:
                    dirnames.remove(ignored_dir)

            for file_name in filenames:
                # If the file has an extension that we are to ignore, skip it.
                if file_name.endswith(ignored_extensions):
                    continue

                try:
                    file_path = os.path.realpath(
                        os.path.join(
                            os.getcwd(),
                            root,
                            file_name,
                        )
                    )
                    with open(file_path) as f:
                        current_line = 0
                        for line in f:
                            current_line += 1
                            if the_regex.search(line):
                                # Get the full path of the file.

                                # Append a tuple containing the following info
                                # to the list of matches:
                                matches.append(
                                    (
                                        # The full line in which the match was
                                        # found.
                                        line,

                                        # relative to the current working
                                        # directory.
                                        file_path.replace(
                                            os.path.abspath("."),
                                            ""
                                        ).replace(
                                            file_name,
                                            ""
                                        ),

                                        # The full filepath, with spaces
                                        # escaped.
                                        file_path.replace(" ", "\\ "),

                                        # The line number on which the match
                                        # occurred.
                                        current_line,
                                    )
                                )
                except IOError:
                    print "IOError on {}".format(file_name)
                    continue

        to_return = []
        for match_tuple in matches:
            # Append a tuple with the following info to the list:
            to_return.append(
                (
                    # [0], the SequenceMatcher ratio, rankin this line against the
                    # original search string.
                    SequenceMatcher(
                        # A one-argument function that takes a sequence element
                        # and returns true iff the element is junk.
                        lambda x: x == " ",
                        # The original string we searched for.
                        search_string,
                        # The full line on which the match was found.
                        match_tuple[0],
                    ).ratio(),

                    # [1], the match tuple itself, containing:
                    #     [1][0] The full line.
                    #     [1][1] The relative file path.
                    #     [1][2] The full file path, with strings escaped.
                    #     [1][3] The line number on which the match occurred.
                    match_tuple,
                )
            )

        # Create and return a new list, based off a sorted and doctored version
        # of the original.
        to_return = [
            "{}:{} | {}".format(
                i[1][2],  # The full file path.
                i[1][3],  # The line number on which the match occurred.
                i[1][0].replace(',', ''),  # The full line of text in the file which matched.
            ).strip() for i in sorted(
                # The original list.
                to_return,
                # A function returning the value to sort on. In this case, the
                # first index of the tuple in the list, which is the
                # SequenceMatcher ratio.
                key=lambda x: x[0],
                # Reverse ordering.
                reverse=True
            )
        ]

        # Pass the list back to the vimscript file.
        vim.command("let l:toReturn = []")
        for i in to_return:
            i = i.replace('"', r'\"')
            the_command = "call add(l:toReturn, \"{}\")".format(i)
            vim.command(the_command)
    else:
        # User didn't enter any input.
        pass

except ImportError, e:
    # There was an error with the imports for this script.
    print "Error while importing from Python within plugin: {}".format(e)
