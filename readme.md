wpers.el --- minor mode for stopping cursor jumping in emacs

Copyright (C) 2015 <wardopdem@gmail.com>

* Authors:         wardopdem@gmail.com
* Version:         2.3
* URL:             <https://github.com/wardopdem/wpers>
* Keywords:        persistent, cursor

Licensed under the [GPL version 3](http://www.gnu.org/licenses/) or later.

# Commentary

This minor mode for people hating cursor's jumping.
Just turn `wpers-mode` or `global-wpers-mode` and cursor
will be moving with saving his position at the line (column).

# Installation & using

Just add wpers.el folder location to load-path and call `require`

        (add-to-list 'load-path <path-to-wpers>)
        (require 'wpers)

Then call command `wpers-mode` for toggle mode in current buffer

        M-x wpers-mode

or `global-wpers-mode` for toggle mode in all buffers

        M-x global-wpers-mode

# Сustomization

Call command `wpers-overlay-visible` or customise `wpers-pspace`
for adjusting visibility of overlay.

Customize `wpers-ovr-killing-funs` to define which functions
reset the vertical position of the cursor (column).

Set the `wpers--remaps` to define the list of functions saving
the vertical position of the cursor (column).

# Note

Mode is compatible with build-in line hilight `hl-line-mode`
but may have problems with other similar modes.
