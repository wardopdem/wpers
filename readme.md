# wpersmode.el --- minor mode for stopping cursor jumping in emacs

Copyright (C) 2015 <wardopdem@gmail.com>

* Authors:         wardopdem@gmail.com
* Keywords:        persistent, cursor
* Package-Version: 20150825.001
* Version:         2.0.0
* URL:             <https://github.com/wardopdem/wpers>

Licensed under the [GPL version 3](http://www.gnu.org/licenses/) or later.

# Commentary

This minor mode for people hating cursor's jumping.
Just turn `wpers-mode` and cursor will be moving
with saving his position at the line (column).

# Installation & using

Just add wpers.el folder location to load-path and call require (f.e. in your init)

        (add-to-list 'load-path <path-to-wpers>)
        (require 'wpers)

Then call command `wpers-mode` for toggle mode

        M-x wpers-mode



