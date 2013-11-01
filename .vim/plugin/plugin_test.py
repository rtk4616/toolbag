try:
    import vim

    print "Hello from within the plugin!"

except ImportError, e:
    print "Error while importing from Python within plugin: {}".format(e)
