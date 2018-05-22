;============================================================================
; OM-pm2
; pm2 sound analysis and synthesis for o7
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================;;;================================================================================================================
; PM2 Preferences
; Author: Jean Bresson - IRCAM 2016 
;================================================================================================================


(in-package :om)

(defmethod default-pm2-path () 
  (let ((libpath (mypathname (find-library "om-pm2"))))
    (om-make-pathname :directory (append (pathname-directory libpath) 
                                         '("resources" "bin")
                                         #+macosx '("mac" "Pm2.app" "Contents" "MacOS") 
                                         #+win32 '("win")
                                         #+linux '("linux")
                                         )
                      :host (pathname-host libpath) :device (pathname-device libpath)
                      :name "pm2" #+win32 :type #+win32 "exe")))

(add-preference-section :libraries "om-pm2" nil '(:pm2-path :pm2-authorize))
(add-preference :libraries :pm2-path "Pm2 exec" :file 'default-pm2-path) 
(add-preference :libraries :pm2-authorize "Authorize" :action 'authorize-pm2)

;;; works for pm2...
(defun forum-authorize (exe-path)
  (if exe-path
    (let ((auth-file (om-choose-file-dialog :prompt "Pleas select the .txt file provided by the ForumNet code generator")))
      (when (and auth-file (probe-file auth-file))
        (om-cmd-line (format nil "~s -init_key_file ~s" 
                             (namestring (real-exec-pathname exe-path))
                             (namestring auth-file)))
        (print "Authorization... done")))
    (om-beep-msg "Executable not found: ~A" exe-path)
  ))

(defmethod! authorize-pm2 ()
   (forum-authorize (get-pref-value :libraries :pm2-path)))
