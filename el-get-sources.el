(setq el-get-sources
      '((:name auctex
               :type http-tar
               :url "http://ftp.gnu.org/pub/gnu/auctex/auctex-11.87.tar.gz"
               :options ("xzf"))
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
	       :features gist)))
