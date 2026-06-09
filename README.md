# magit-psr

Show PHP PSR errors from `phpcs` in the `magit-status` buffer.

Inspired by [magit-todos](https://github.com/alphapapa/magit-todos), but for PHP CodeSniffer violations.

## Requirements

- Emacs 26.1 or later
- [magit](https://github.com/magit/magit) 2.13+
- [phpcs](https://github.com/squizlabs/PHP_CodeSniffer) (PHP_CodeSniffer)

## Usage

```elisp
M-x magit-psr-mode
```

Then open `M-x magit-status`. The "PSR" section appears at the bottom.

Press `RET` on an error to jump to its location.

To refresh manually: `M-x magit-psr-update`.

## Customization

```
M-x customize-group magit-psr
```

| Variable                  | Default                       | Description                                    |
|---------------------------|-------------------------------|------------------------------------------------|
| `magit-psr-executable`    | `"phpcs"`                     | Path to the phpcs executable                   |
| `magit-psr-standard`      | `"PSR12"`                     | Coding standard (PSR1, PSR2, PSR12, etc.)      |
| `magit-psr-show-warnings` | `nil`                         | Show warnings in addition to errors            |
| `magit-psr-exclude-globs` | `("vendor/" "node_modules/")` | Glob patterns to exclude                       |
| `magit-psr-max-items`     | `20`                          | Collapse section when exceeding N items        |
| `magit-psr-depth`         | `nil`                         | Maximum subdirectory depth (`nil` = unlimited) |
| `magit-psr-phpcs-args`    | `nil`                         | Extra arguments for phpcs                      |
| `magit-psr-insert-after`  | `(bottom)`                    | Where to insert the section in the buffer      |

## License

GPLv3
