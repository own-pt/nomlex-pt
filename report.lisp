
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
	      (report-rel noum noum-senses out)))))))


(defun report-1 (norms filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (dolist (n norms)
      (let* ((verb (getf (cdr n) :VERB))
	     (noum (getf (cdr n) :ORTH))
	     (verb-senses (get-senses verb :lst '(!wn30:VerbSynset)))
	     (noum-senses (get-senses noum))
	     (verb-size (length verb-senses))
	     (noum-size (length noum-senses)))
	(format out "; ~a:~a, ~a:~a~%" verb verb-size noum noum-size)
	(if (equal verb-size 0)
	    (format out "~s~%" `(synset ? ,verb)))
	(if (equal noum-size 0)
	    (format out "~s~%" `(synset ? ,noum)))))))


;;; sample sentences

(defparameter *preds* '(!wn30:hypernymOf !wn30:memberHolonymOf !wn30:instanceOf !wn30:substanceHolonymOf
			!wn30:entails !wn30:causes))


(defun random-element (list)
  "Return some element of the list, chosen at random."
  (nth (random (length list)) list))


(defun get-word (ss)
  (select0 (?lex)
    (:reorder t)
    (:use-planner t)
    (q- (?? ss) !wn30:synsetId ?id !source:wordnet-en.rdf)
    (q- (?? ss) !rdf:type ?type !source:wordnet-en.rdf)
    (q- ?sspt !wn30:synsetId ?id !source:wordnet-br.rdf)
    (q- ?sspt !rdf:type ?type !source:wordnet-br.rdf)
    (q- ?sspt !wn30:containsWordSense ?ws)
    (q- ?ws !wn30:word ?word)
    (q- ?word !wn30:lexicalForm ?lex)))


(defun sample (filename size preds)
  (with-open-file (out filename :direction :output :if-exists :supersede) 
    (dolist (pred preds)
      (let ((a-triples (get-triples-list :p pred :limit nil))
	    (counter 0))
	(do* ((a-triple (random-element a-triples)
			(random-element a-triples))
	      (inter 1 (+ 1 inter))
	      (words-s (get-word (subject a-triple))
		       (get-word (subject a-triple)))
	      (words-o (get-word (object a-triple))
		       (get-word (object a-triple))))
	     ((>= counter size))
	  (if (not (or (null words-s) (null words-o)))
	      (let ((word1 (car (random-element (get-word (subject a-triple)))))
		    (word2 (car (random-element (get-word (object a-triple))))))
		(incf counter)
		(format out "; [~a/~a] ~a ~a ~a~%" counter inter word1 pred word2)
		(format out "~s~%~%" 
			`(sentence ? ,pred
				   ,(part->terse (subject a-triple)) 
				   ,(part->value word1) 
				   ,(part->terse (object a-triple))
				   ,(part->value word2))))))))))


(sample 10 *preds*)

