# flymake-jsonlint
A Flymake backend for validating JSON files for Emacs (26+), using
[jsonlint](https://www.npmjs.com/package/jsonlint)

## Installation
`flymake-jsonlint` is available on MELPA, so you can add it using your
preferred Emacs package manager or manually to your `load-path`.

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
