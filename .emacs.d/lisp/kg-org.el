(defun kg/time-add-days (time days)
  (let* ((decoded-time (decode-time time))
         (year         (nth 5 decoded-time))
         (month        (nth 4 decoded-time))
         (day          (nth 3 decoded-time)))
    (encode-time 0 0 0 (+ day days) month year)))

(defun kg/time-get-day-of-week (time)
  (nth 6 (decode-time time)))

(defun kg/time-get-week-of-year (time)
  (nth 6 (decode-time time)))

(defun kg/org-week-day-title (time)
  (format-time-string "%A - %b %-d" time))

(defun kg/org-week-day-format-template (time)
  (format "\n* %s\n** Tasks\n** Journal"
          (kg/org-week-day-title time)))

(defun kg/org-week-format-template (time)
  (let* ((first-day (kg/time-add-days time (- (dw/time-get-day-of-week time))))
         (last-day (kg/time-add-days first-day 6))
         (title (format "#+TITLE: Week %s - %s to %s"
                        (format-time-string "%U" first-day)
                        (format-time-string "%B %d" first-day)
                        (format-time-string "%B %d" last-day)))
         (days (string-join (mapcar (lambda (dow)
                                      (kg/org-week-day-format-template
                                        (kg/time-add-days first-day dow)))
                                    '(0 1 2 3 4 5 6)))))
    (format "%s\n\n* Goals\n** Work\n** Personal%s\n* Review" title days)))

(defun kg/org-week-file-name (time)
  (format-time-string "%Y/%Y-Week-%U.org" time))

(defun kg/org-week-find-file (time)
  (let* ((week-file (concat "~/Notes/Journal/" (kg/org-week-file-name time)))
         (file-exists (file-exists-p week-file)))
    (unless file-exists
      (make-directory (file-name-directory week-file) t))
    (find-file week-file)
    (unless file-exists
      ;; Populate the file with initial contents
      (goto-char (point-min))
      (insert (kg/org-week-format-template time))
      (goto-char (point-min))
      (org-overview))))

(defun kg/org-week-today-focus-heading (title)
  ;; (find-file (expand-file-name org-file))
  (goto-char (point-min))
  (search-forward (concat "* " (kg/org-week-day-title nil)))
  (search-forward (concat "** " title))
  (org-overview)
  (org-reveal)
  (org-show-subtree)
  (forward-line))

(defun kg/org-week-plan-today ()
  (interactive)
  (kg/org-week-find-file (current-time))
  (goto-char (point-min))
  (org-overview)
  (search-forward "* Goals")
  (org-show-subtree)
  (search-forward (concat "* " (kg/org-week-day-title nil)))
  (org-show-subtree)
  (search-forward "** Tasks")
  (forward-line))

(defun kg/org-week-focus-today ()
  (interactive)
  (kg/org-week-find-file (current-time))
  (goto-char (point-min))
  (org-overview)
  (search-forward (concat "* " (kg/org-week-day-title nil)))
  (org-show-children 3)
  (org-narrow-to-subtree))

(provide 'kg-org)
