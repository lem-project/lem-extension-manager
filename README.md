# LEM (Lem Extension Manager)

It's a configuration library that add the ability to Lem to have packages withing the user configuration directory.

## Usage

### Init example
The main function that we can use to add new packages is `lem-use-package`, this fuctions allow us to add libraries
that will be loaded at run time when open Lem (the performance shouldn't be too much of a problem, as sbcl is very good using the cache).

To use this function, we can modify our `init.lisp` file in this way:
 - Using the git source (we can specify a branch or specific commit):


```lisp
(lem-extension-manager:lem-use-package
 "lisp-critic"
 :source '(:type :git
           :url "https://github.com/g000001/lisp-critic.git"))
		   
(lem-extension-manager:lem-use-package "versioned-objects"
                 :source '(:type :git
                           :url "https://github.com/smithzvk/Versioned-Objects.git"
                           :branch "advance-versioning"))
```
 - Using the quicklisp package manager directly:

```lisp

(lem-extension-manager:lem-use-package "fiveam" :source (:type :quicklisp))

```

### Interactive commands

LEM also provides some interactive commands to add/remove extension:

- `extension-manager-install-ql-package`:

This command will prompt for a list of all quicklisp indexed libraries to be installed, you can choose any of them and LEM will install it on your
configuration (keep in mind that even tho it will get install/indexed for LEM, it will NOT be loaded unless specify in the configuration).


- `extension-manager-test-ql-package`:

This command is similar to `extension-manager-install-ql-package` with the key difference that is intended to try out quicklisp packages, so the command will download the package to a temporary directory, loaded into Lem and then forget about it. It's inteded to test packages on the fly, to make it permanent, use the `lem-use-package` function.

- `extension-manager-remove-package`:

This command will prompt for the installed packages and will removed the selected one.

- `extension-manager-purge-packages`:

This command will remove ALL installed packages.



## Configuration

The packages are installed on returned folder from the lem function `lem-extension-manager::default-home` by default, but can be changed by modifing the variable `lem-extension-manager:*packages-directory*`.




## Internals
TODO
