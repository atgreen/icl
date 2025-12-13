## Install Quicklisp

curl -O https://beta.quicklisp.org/quicklisp.lisp

if [ ! -d "$HOME/quicklisp" ]; then
    sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(quit)"
fi

## Install dependencies

sbcl --load scripts/dep-install.lisp --eval "(quit)"

## Move the project to local-projects

mkdir -p $HOME/quicklisp/local-projects

cp scripts/lisp-init $HOME/.sbclrc 

cp -R ../slynk-client $HOME/quicklisp/local-projects/ 
