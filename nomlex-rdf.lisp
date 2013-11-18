
(require :agraph "/Users/arademaker/agraph-4.11-acl90smp-client/agraph4.fasl")
(ql:quickload :fare-csv)
(ql:quickload :cl-ppcre)

(enable-!-reader)
(enable-print-decoded t)

(register-namespace "wn30"      "http://arademaker.github.com/wn30/schema/" :errorp nil)
(register-namespace "wn30en"    "http://arademaker.github.com/wn30/instances/" :errorp nil)
(register-namespace "wn30br"    "http://arademaker.github.com/wn30-br/instances/" :errorp nil)
(register-namespace "nomlex"    "http://arademaker.github.com/nomlex-br/schema/" :errorp nil)
(register-namespace "nomlex-br" "http://arademaker.github.com/nomlex-br/instances/" :errorp nil)

(defparameter *types* '(("lex"    . !nomlex:LexicalNominalization) 
			("agent"  . !nomlex:AgentNominalization)
			("result" . !nomlex:ResultNominalization)
			("event"  . !nomlex:EventNominalization)))

(defun get-elem (n reg)
  (cl-ppcre:regex-replace-all " " (nth n reg) "_"))

(defun add-word (res str)
  (add-triple res !rdf:type !wn30:Word)
  (add-triple res !wn30:lexicalForm (literal str :language "pt")))

(defun read-registers (filename)
  (let ((regs (fare-csv:read-csv-file filename))) 
    (dolist (r (cdr regs))
      (let ((node (resource (format nil "nomlex-~a-~a" (get-elem 0 r) (get-elem 1 r)) "nomlex-br"))
	    (a-verb (resource (format nil "word-~a" (get-elem 0 r)) "wn30br"))
	    (a-noun (resource (format nil "word-~a" (get-elem 1 r)) "wn30br")))
	(add-triple node !nomlex:verb a-verb)
	(add-triple node !nomlex:noun a-noun)
	(add-word a-verb (nth 0 r))
	(add-word a-noun (nth 1 r))
	(add-triple node !rdf:type !nomlex:Nominalization)
	(if (not (string= (nth 2 r) "NA")) 
	    (add-triple node !nomlex:plural (literal (nth 2 r) :language "pt")))
	(dolist (type (cl-ppcre:split ";" (nth 4 r)))
	  (if (and (not (string= (nth 4 r) "NA")) 
		   (assoc type *types* :test #'string=))
	      (add-triple node !rdf:type (cdr (assoc type *types* :test #'string=)))))
	(if (not (string= (nth 3 r) "NA"))
	    (let ((prov (car (cl-ppcre:split ":" (nth 3 r)))))
	      (add-triple node !dc:provenance (literal prov :language "pt"))))))))


