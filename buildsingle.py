from subprocess import check_output

import os

os.chdir(os.path.realpath(os.path.dirname(__file__)))

template = '''
<!doctype>
<html>
<head>
  <title>Set</title>
  <style type="text/css">{css}</style>
</head>
<body>
  <div id="app" style="width: 100%; height: 100%;"></div>
  <script type="text/javascript">{javascript}</script>
</body>
</html>
'''

css = open('index.css').read()
javascript = check_output('./node_modules/.bin/uglifyjs bundle.js', shell=True).decode("utf-8")

with open('bundled.index.html', 'w') as f:
    f.write(template.format(css=css, javascript=javascript))
