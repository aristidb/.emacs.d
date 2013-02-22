(setq el-get-sources
      '((:name auctex
               :type http-tar
               :url "http://ftp.gnu.org/pub/gnu/auctex/auctex-11.87.tar.gz"
               :options ("xzf")
               :build `(("./configure"
                         "--without-texmf-dir"
                         "--with-lispdir=`pwd`"
                         ,(concat "--with-emacs=" el-get-emacs))
                        "make")
               :load-path ("." "preview")
               :load  ("tex-site.el" "preview/preview-latex.el")
               :info "doc")
        (:name ess
               :description "Emacs Speaks Statistics: statistical programming within Emacs"
               :type http-tar
               :url "http://ess.r-project.org/downloads/ess/ess-12.09-2.tgz"
               :options ("xzf")
               :info "doc/info/"
               :build `,(mapcar
                         (lambda (target)
                           (concat "make " target " EMACS=" el-get-emacs))
                         '("all"))
               :load-path ("lisp")
               :features ess-site)
	(:name pcache
	       :description "persistent caching for Emacs"
	       :type github
	       :pkgname "sigma/pcache")
	(:name logito
	       :description "tiny logging framework for Emacs"
	       :type github
	       :pkgname "sigma/logito")
	(:name gh
	       :description "Github API for Emacs"
	       :depends (pcache logito)
	       :type github
	       :pkgname "sigma/gh.el"
	       :features gh)
	(:name gist
	       :description "Gist for Emacs"
	       :depends gh
	       :type github
	       :pkgname "defunkt/gist.el"
	       :features gist)
	(:name unbound
	       :description "find convenient unbound keystrokes"
	       :type elpa
	       :repo ("marmalade" . "http://marmalade-repo.org/packages/"))
        (:name s
               :description "The long lost Emacs string manipulation library."
               :type github
               :username "magnars"
               :pkgname "s.el")
	(:name dash
	       :description "A modern list library for Emacs"
	       :type github
	       :username "magnars"
	       :pkgname "dash.el")
        (:name projectile
               :description "Project Interaction Library for Emacs"
	       :depends (s dash)
               :type github
               :username "bbatsov"
               :pkgname "projectile")
        (:name sauron
               :depends (alert org-mode))
        (:name weechat
               :description "Weechat for Emacs yay"
               :depends (sauron cl-lib s)
               :type github
               :username "the-kenny"
               :pkgname "weechat.el"
               :features weechat)))

(if (version-list-< (version-to-list emacs-version) '(24 3 0))
    (add-to-list 'el-get-sources '(:name cl-lib :type elpa))
  (add-to-list 'el-get-sources '(:name cl-lib :type builtin)))
