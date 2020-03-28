;;; adh_gcmh.el --- Configuration for garbage collection magic hack -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Mon 06 May 2019 23:09

;;; Commentary:
;;
;; This setup employs the gcmh package.
;;
;; Save considerable time, and make emacs more responsive and snappier, by
;;reducing how regularly garbage collection is performed while running code (by
;;setting a very high threshold). Additionally, utilise 'dead time', while emacs
;;isn't being used, to perform garbage collection.

;;; Code:

(require 'gcmh)

(gcmh-mode 1)

(setq garbage-collection-messages t)

(provide 'adh_gcmh)
;;; adh_gcmh.el ends here
