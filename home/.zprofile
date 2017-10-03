eval $(ssh-agent)
ssh-add -l >/dev/null || alias ssh='ssh-add -l >/dev/null || ssh-add && unalias ssh; ssh'

export PATH="$HOME/.cargo/bin:$HOME/.node_modules/bin:$PATH"
