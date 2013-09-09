
(ql:quickload :cl-ppcre)

(defparameter *norms* (with-open-file (data #P"/tmp/nomlex-br.lisp")
			(loop for norm = (read data nil nil)
			      until (null norm)
			      collect norm)))

(defparameter *default-filter* (list !wn30:NounSynset !wn30:AdjectiveSynset !wn30:AdverbSynset
				     !wn30:AdjectiveSatelliteSynset))


(defun get-senses (lexical &key (lst *default-filter*))
  (flet ((senses (lex)
	   (select0-distinct (?ws ?type) 
	     (q- ?w !wn30:lexicalForm (?? (literal lex)))
	     (q- ?ws !wn30:word ?w)
	     (q- ?ss !wn30:containsWordSense ?ws)
	     (q- ?ss !rdf:type ?type))))
    (multiple-value-bind (word matches)
	(cl-ppcre:scan-to-strings "([a-zA-Z]+)([0-9]?)" lexical)
      (declare (ignore word))
      (let ((w (if (equal (elt matches 1) "") 
		   lexical 
		   (elt matches 0))))
	(remove-if-not (lambda (tl) (member (cadr tl) lst :test #'part=))
		       (senses w))))))


(defun fmt-part (a-part)
  (cl-ppcre:regex-replace "wordsense-" (part->terse a-part) ""))

(defun fmt-tuple (a-tuple)
  (format nil "~a/~a" 
	  (part->value (cadr a-tuple)) (fmt-part (car a-tuple))))

(defun get-rels (ws)
  (select0 (?ws1 ?lf)
    (q- (?? ws) !wn30:derivationallyRelated ?ws1)
    (q- ?ws1 !wn30:word ?w)
    (q- ?w !wn30:lexicalForm ?lf)))

(defun report-rel (lex ws-list stream)
  (dolist (ws ws-list)
    (format stream "  ~a/~a: ~{~a~^ ~}~%" 
	    lex (fmt-part (car ws)) 
	    (mapcar #'fmt-tuple (get-rels (car ws))))))

(defun report (norms filename &key (expand nil))
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (dolist (n norms)
      (let* ((verb (getf (cdr n) :VERB))
	     (noum (getf (cdr n) :ORTH))
	     (verb-senses (get-senses verb :lst '(!wn30:VerbSynset)))
	     (noum-senses (get-senses noum)))
	(format out "~a:~a, ~a:~a~%" 
		verb (length verb-senses) noum (length noum-senses))
	(if expand 
	    (progn 
	      (report-rel verb verb-senses out)
	      (report-rel noum noum-senses out))))))))
