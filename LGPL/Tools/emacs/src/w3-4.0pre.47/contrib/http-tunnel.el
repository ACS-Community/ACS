(defvar http-tunnel-host "firewallmachine")
(defvar http-tunnel-port 80)

(defun open-http-tunneled-connection (name buffer host service)
  (let ((proc (open-network-stream name buffer http-tunnel-host http-tunnel-port))
	(need-to-spin t))
    (if (or (null proc) (not (memq (process-status proc) '(run open))))
	(error "Could not open connection"))
    (process-send-string proc (format (eval-when-compile
					(concat
					 "CONNECT %s:%s HTTP/1.0\r\n"
					 "User-Agent: Emacs/%d.%d\r\n"
					 "\r\n"))
				      host service
				      emacs-major-version
				      emacs-minor-version))
    (save-excursion
      (set-buffer buffer)
      (while (and (memq (process-status proc) '(open run)) need-to-spin)
	(accept-process-output proc 3)
	(goto-char (point-min))
	(if (re-search-forward "\r\n\r\n" nil t)
	    (progn
	      (delete-region (point-min) (match-end 0))
	      (setq need-to-spin nil)))
	(goto-char (point-max))))
    (if (not (memq (process-status proc) '(open run)))
	(error "Could not open connection"))
    proc))

(let ((socks-noproxy '(".*")))
  (setq x (open-http-tunneled-connection "x" "x" "www.cs.indiana.edu" 80))
  (process-send-string x "GET / HTTP/1.0\r\n\r\n"))
