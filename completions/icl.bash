# Bash completion for ICL (Interactive Common Lisp)
#
# Install to /usr/share/bash-completion/completions/icl
# or source from ~/.bashrc

_icl_completions() {
    local cur prev words cword
    _init_completion -s 2>/dev/null || {
        COMPREPLY=()
        cur="${COMP_WORDS[COMP_CWORD]}"
    }

    # Get completions from ICL itself
    local suggestions
    suggestions=$(icl --bash-completions 2>/dev/null)

    # Handle options that take values
    case "${prev}" in
        --lisp)
            COMPREPLY=($(compgen -W "sbcl ccl ecl clisp abcl clasp roswell" -- "${cur}"))
            return
            ;;
        --load|-l)
            # Complete Lisp files
            COMPREPLY=($(compgen -f -X '!*.lisp' -- "${cur}") $(compgen -f -X '!*.lsp' -- "${cur}") $(compgen -d -- "${cur}"))
            return
            ;;
        --connect|--mcp-server)
            # Suggest common host:port patterns
            COMPREPLY=($(compgen -W "localhost:4005 127.0.0.1:4005" -- "${cur}"))
            return
            ;;
    esac

    # Complete options
    if [[ "${cur}" == -* ]]; then
        COMPREPLY=($(compgen -W "${suggestions}" -- "${cur}"))
    fi
}

complete -o default -o bashdefault -F _icl_completions icl
