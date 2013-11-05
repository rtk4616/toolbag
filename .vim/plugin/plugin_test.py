try:
    import vim
    import sys

    print "inside the vim plugin, sys.argv is {}".format(sys.argv)

except ImportError, e:
    print "Error while importing from Python within plugin: {}".format(e)
