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
               :features ess-site)))
