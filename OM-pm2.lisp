;;;===================================================
;;;
;;; OM-pm2
;;; pm2 additive analysis/synthesis in OpenMusic
;;;
;;; Requires perliminary installation of pm2 (also included in AudioSculpt)
;;; Set pm2 path in OM preferences (externals) once the library is loaded.
;;;
;;;
;;; LIBRARY MAIN FILE
;;; Author: Jean Bresson (IRCAM 2006-2018)
;;;
;;;===================================================


(in-package :om)

(compile&load (namestring (om-relative-path '("sources") "package")))
(compile&load (namestring (om-relative-path '("sources") "pm2-additive")))
(compile&load (namestring (om-relative-path '("sources") "om-functions")))
(compile&load (namestring (om-relative-path '("sources") "om6-preferences")))

  (om::fill-library 
 '(("Analysis" nil nil (partial-tracking chord-seq-analysis pm2-f0) nil)
   ("Synthesis" nil nil (pm2-add-synth pm2-subtract) nil)
   ))


(unless (fboundp 'om::set-lib-release) (defmethod om::set-lib-release (version &optional lib) nil))


(om::set-lib-release 1.6)

(print "
;;;===========================================================================
;;; OM-pm2 1.6
;;; pm2 additive analysis/synthesis in OpenMusic
;;;
;;; (c) IRCAM 2006-2018
;;;===========================================================================
")
