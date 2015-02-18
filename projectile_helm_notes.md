* C-c p f a : find other file (h->cpp). Works well, bit slow.
* helm-semantic-or-imenu : lists symbols and lets you navigate. same
  as ecb, but in helm.
* C-c p f : find file. OK-ish. Need to set up exclusion lists, I guess.
* C-c p j / projectile-find-tag : find symbol. uses IDO, which may not
  be appropriate, cause you usually want to see the file it's in as
  well. So need to configure this to use Helm.
* helm-gtags-find-symbol : doesn't open a helm buffer. doesn't seem to
  find symbols. not sure if gtags file is broken, or if it doesn't
  find the file, or if it's also case sensitivity. needs debugging,
  starting with getting a helm buffer showing.
* helm-projectile-ag : works a treat, but is case sensitive
