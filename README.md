# flymake-jsonlint
A Flymake backend for validating JSON files for Emacs (26+), using
[python json tool](https://docs.python.org/3/library/json.html#module-json.tool)

> [!IMPORTANT]
> flymake-jsonlint currently expects English outputs from Python json.tool cli module.

## Installation
`flymake-jsonlint` is available in MELPA repositories.

## Usage
Make sure to have `json-mode` or similar installed before adding this
backend, and have it configured to open your JSON files if it doesn't
do that automatically.

Add the following to your `.emacs` file or other init file for Emacs
to load the backend when visiting a JSON file using `json-mode`.

```elisp
(add-hook json-mode-hook 'flymake-jsonlint-setup)
```

Remember to enable `flymake-mode` as well, preferably after.

## License

Distributed under the GNU General Public License, version 3.
