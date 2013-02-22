(setq el-get-sources
      '((:name auctex
               :type elpa)
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
	(:name gh
	       :description "Github API for Emacs"
	       :type github
	       :pkgname "sigma/gh.el"
	       :features gh)
	(:name gist
	       :description "Gist for Emacs"
	       :depends gh
	       :type github
	       :pkgname "defunkt/gist.el"
	       :features gist)))
