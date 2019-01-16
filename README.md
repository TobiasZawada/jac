# Just Another buffer Cloning method (jac.el)

This is like `clone-indirect-buffer` but it only clones the text -- not the text properties.

# Installation

Put `jac.el` somewhere in your `load-path`. Maybe compile it.
Add the following line to your init file:

    (autoload 'jac "jac")

# Usage

Call the command `jac` for cloning the current buffer.

Both buffers have the same text contents.  If you edit the text in the
original buffer the modifications are transferred to the clone and
visa-versa.  Text properties are not transferred.  That means you
can switch the major mode for the clone.
