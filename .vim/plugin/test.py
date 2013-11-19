with open("plugin_test.py") as f:
    lines = [line.strip() for line in f.readlines() if line.strip()]
    for line in lines:
        if "import" in line:
            print line.strip()
