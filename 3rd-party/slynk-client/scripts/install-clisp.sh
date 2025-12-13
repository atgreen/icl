## Install Quicklisp

curl -O https://beta.quicklisp.org/quicklisp.lisp

clisp -x '(load "quicklisp.lisp") (quicklisp-quickstart:install)'

## Install dependencies

clisp -x '(load "scripts/dep-install.lisp")'

## Move the project to local-projects

mkdir -p $HOME/quicklisp/local-projects

cp scripts/lisp-init $HOME/.clisprc 

cp -R ../slynk-client $HOME/quicklisp/local-projects/ 
