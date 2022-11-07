#lang racket
(require csc151)

;Authors: Jiyoung Chang, Chase Holdener, and John Tobin
;Generating Classical Music Melodies

;------------------------------------------------------------------------------------------------------------

;;;Procedure
;;;   clean-notes
;;;Parameters
;;;   file, a file path as a string 
;;;Purpose
;;;   to make the file information into a workable list by removing the /t that appears when
;;;    file->lines is run and turning the strings into numbers.
;;;Produces
;;;   result, a list of lists that each contain sequential timestamps and the frequency playing in the song at that timestamp.
;;;Preconditions
;;;   file must be a valid file location
;;;Postconditions
;;;   (car (list-ref (- (length result) 1))) will be the total length of the song in seconds
;;;   (- (length result) 1) is the length of the song times 100
(define clean-notes
 (lambda (file)
   (let ([clean-frequencies-helper (lambda (lst)
                                     (append (list (string->number (car lst))) (list (string->number (cadr lst)))))])
 (map frequencies->notes (map clean-frequencies-helper (map (section string-split <> "\t") (file->lines file)))))))

;;; Procedure:
;;;  frequencies->notes
;;; Parameters:
;;;  lst, a list
;;; Purpose:
;;;  generates a certain range of frequencies into notes
;;; Produces:
;;;  result, a list of lists
;;; Preconditions:
;;;  lst has to have at least 2 elements in which the second element is a nonnegative real number 
;;; Postconditions:
;;;  If lst is a list that has only one element, it will return an error
;;;  If lst is a list that has 2 or more elements, but the second element is not a nonnegative real number, it will return an error
;;;  If lst is a list that has 2 or more elements, and the second element is a nonnegative real number,
;;;   then result will be a list that has the same elements as lst except that the second element will become a string
(define frequencies->notes
  (lambda (lst)
    (cond
      [(equal? (cadr lst) 0) (append (list (car lst)) (list "rest"))]
      [(and (> (cadr lst) 0) (<= (cadr lst) 16.835)) (append (list (car lst)) (list "C0"))]
      [(and (> (cadr lst) 16.835) (<= (cadr lst) 17.835)) (append (list (car lst)) (list "C#0"))]
      [(and (> (cadr lst) 17.835) (<= (cadr lst) 18.9)) (append (list (car lst)) (list "D0"))]
      [(and (> (cadr lst) 18.9) (<= (cadr lst) 20.025)) (append (list (car lst)) (list "Eb0"))]
      [(and (> (cadr lst) 20.025) (<= (cadr lst) 21.215)) (append (list (car lst)) (list "E0"))]
      [(and (> (cadr lst) 21.215) (<= (cadr lst) 22.475)) (append (list (car lst)) (list "F0"))]
      [(and (> (cadr lst) 22.475) (<= (cadr lst) 23.81)) (append (list (car lst)) (list "F#0"))]
      [(and (> (cadr lst) 23.81) (<= (cadr lst) 25.23)) (append (list (car lst)) (list "G0"))]
      [(and (> (cadr lst) 25.23) (<= (cadr lst) 26.73)) (append (list (car lst)) (list "Ab0"))]
      [(and (> (cadr lst) 26.73) (<= (cadr lst) 28.32)) (append (list (car lst)) (list "A0"))]
      [(and (> (cadr lst) 28.32) (<= (cadr lst) 30.005)) (append (list (car lst)) (list "Bb0"))]
      [(and (> (cadr lst) 30.005) (<= (cadr lst) 31.785)) (append (list (car lst)) (list "B0"))]
      [(and (> (cadr lst) 31.785) (<= (cadr lst) 33.675)) (append (list (car lst)) (list "C1"))]
      [(and (> (cadr lst) 33.675) (<= (cadr lst) 35.68)) (append (list (car lst)) (list "C#1"))]
      [(and (> (cadr lst) 35.68) (<= (cadr lst) 37.8)) (append (list (car lst)) (list "D1"))]
      [(and (> (cadr lst) 37.8) (<= (cadr lst) 40.045)) (append (list (car lst)) (list "Eb1"))]
      [(and (> (cadr lst) 40.045) (<= (cadr lst) 42.425)) (append (list (car lst)) (list "E1"))]
      [(and (> (cadr lst) 42.425) (<= (cadr lst) 44.95)) (append (list (car lst)) (list "F1"))]
      [(and (> (cadr lst) 44.95) (<= (cadr lst) 47.625)) (append (list (car lst)) (list "F#1"))]
      [(and (> (cadr lst) 47.625) (<= (cadr lst) 50.455)) (append (list (car lst)) (list "G1"))]
      [(and (> (cadr lst) 50.455) (<= (cadr lst) 53.455)) (append (list (car lst)) (list "Ab1"))]
      [(and (> (cadr lst) 53.455) (<= (cadr lst) 56.635)) (append (list (car lst)) (list "A1"))]
      [(and (> (cadr lst) 56.635) (<= (cadr lst) 60.005)) (append (list (car lst)) (list "Bb1"))]
      [(and (> (cadr lst) 60.005) (<= (cadr lst) 63.575)) (append (list (car lst)) (list "B1"))]
      [(and (> (cadr lst) 63.575) (<= (cadr lst) 67.355)) (append (list (car lst)) (list "C2"))]
      [(and (> (cadr lst) 67.355) (<= (cadr lst) 71.36)) (append (list (car lst)) (list "C#2"))]
      [(and (> (cadr lst) 71.36) (<= (cadr lst) 75.6)) (append (list (car lst)) (list "D2"))]
      [(and (> (cadr lst) 75.6) (<= (cadr lst) 80.095)) (append (list (car lst)) (list "Eb2"))]
      [(and (> (cadr lst) 80.095) (<= (cadr lst) 84.86)) (append (list (car lst)) (list "E2"))]
      [(and (> (cadr lst) 84.86) (<= (cadr lst) 89.905)) (append (list (car lst)) (list "F2"))]
      [(and (> (cadr lst) 89.905) (<= (cadr lst) 95.25)) (append (list (car lst)) (list "F#2"))]
      [(and (> (cadr lst) 95.25) (<= (cadr lst) 100.915)) (append (list (car lst)) (list "G2"))]
      [(and (> (cadr lst) 100.915) (<= (cadr lst) 106.915)) (append (list (car lst)) (list "Ab2"))]
      [(and (> (cadr lst) 106.915) (<= (cadr lst) 113.27)) (append (list (car lst)) (list "A2"))]
      [(and (> (cadr lst) 113.27) (<= (cadr lst) 120.005)) (append (list (car lst)) (list "Bb2"))]
      [(and (> (cadr lst) 120.005) (<= (cadr lst) 127.14)) (append (list (car lst)) (list "B2"))]
      [(and (> (cadr lst) 127.14) (<= (cadr lst) 134.7)) (append (list (car lst)) (list "C3"))]
      [(and (> (cadr lst) 134.7) (<= (cadr lst) 142.71)) (append (list (car lst)) (list "C#3"))]
      [(and (> (cadr lst) 142.71) (<= (cadr lst) 151.195)) (append (list (car lst)) (list "D3"))]
      [(and (> (cadr lst) 151.195) (<= (cadr lst) 160.185)) (append (list (car lst)) (list "Eb3"))]
      [(and (> (cadr lst) 160.185) (<= (cadr lst) 169.71)) (append (list (car lst)) (list "E3"))]
      [(and (> (cadr lst) 169.71) (<= (cadr lst) 179.805)) (append (list (car lst)) (list "F3"))]
      [(and (> (cadr lst) 179.805) (<= (cadr lst) 190.5)) (append (list (car lst)) (list "F#3"))]
      [(and (> (cadr lst) 190.5) (<= (cadr lst) 201.825)) (append (list (car lst)) (list "G3"))]
      [(and (> (cadr lst) 201.825) (<= (cadr lst) 213.825)) (append (list (car lst)) (list "Ab3"))]
      [(and (> (cadr lst) 213.825) (<= (cadr lst) 226.54)) (append (list (car lst)) (list "A3"))]
      [(and (> (cadr lst) 226.54) (<= (cadr lst) 240.01)) (append (list (car lst)) (list "Bb3"))]
      [(and (> (cadr lst) 240.01) (<= (cadr lst) 254.285)) (append (list (car lst)) (list "B3"))]
      [(and (> (cadr lst) 254.285) (<= (cadr lst) 269.405)) (append (list (car lst)) (list "C4"))]
      [(and (> (cadr lst) 269.405) (<= (cadr lst) 285.42)) (append (list (car lst)) (list "C#4"))]
      [(and (> (cadr lst) 285.42) (<= (cadr lst) 302.395)) (append (list (car lst)) (list "D4"))]
      [(and (> (cadr lst) 302.395) (<= (cadr lst) 320.38)) (append (list (car lst)) (list "Eb4"))]
      [(and (> (cadr lst) 320.38) (<= (cadr lst) 339.43)) (append (list (car lst)) (list "E4"))]
      [(and (> (cadr lst) 339.43) (<= (cadr lst) 359.61)) (append (list (car lst)) (list "F4"))]
      [(and (> (cadr lst) 359.61) (<= (cadr lst) 380.995)) (append (list (car lst)) (list "F#4"))]
      [(and (> (cadr lst) 380.995) (<= (cadr lst) 403.65)) (append (list (car lst)) (list "G4"))]
      [(and (> (cadr lst) 403.65) (<= (cadr lst) 427.65)) (append (list (car lst)) (list "Ab4"))]
      [(and (> (cadr lst) 427.65) (<= (cadr lst) 453.08)) (append (list (car lst)) (list "A4"))]
      [(and (> (cadr lst) 453.08) (<= (cadr lst) 480.02)) (append (list (car lst)) (list "Bb4"))]
      [(and (> (cadr lst) 480.02) (<= (cadr lst) 508.565)) (append (list (car lst)) (list "B4"))]
      [(and (> (cadr lst) 508.565) (<= (cadr lst) 538.81)) (append (list (car lst)) (list "C5"))]
      [(and (> (cadr lst) 538.81) (<= (cadr lst) 570.85)) (append (list (car lst)) (list "C#5"))]
      [(and (> (cadr lst) 570.85) (<= (cadr lst) 604.79)) (append (list (car lst)) (list "D5"))]
      [(and (> (cadr lst) 604.79) (<= (cadr lst) 640.75)) (append (list (car lst)) (list "Eb5"))]
      [(and (> (cadr lst) 640.75) (<= (cadr lst) 678.855)) (append (list (car lst)) (list "E5"))]
      [(and (> (cadr lst) 678.855) (<= (cadr lst) 719.225)) (append (list (car lst)) (list "F5"))]
      [(and (> (cadr lst) 719.225) (<= (cadr lst) 761.99)) (append (list (car lst)) (list "F#5"))]
      [(and (> (cadr lst) 761.99) (<= (cadr lst) 807.3)) (append (list (car lst)) (list "G5"))]
      [(and (> (cadr lst) 807.3) (<= (cadr lst) 855.305)) (append (list (car lst)) (list "Ab5"))]
      [(and (> (cadr lst) 855.305) (<= (cadr lst) 906.165)) (append (list (car lst)) (list "A5"))]
      [(and (> (cadr lst) 906.165) (<= (cadr lst) 960.05)) (append (list (car lst)) (list "Bb5"))]
      [(and (> (cadr lst) 960.05) (<= (cadr lst) 1017.135)) (append (list (car lst)) (list "B5"))]
      [(and (> (cadr lst) 1017.135) (<= (cadr lst) 1077.615)) (append (list (car lst)) (list "C6"))]
      [(and (> (cadr lst) 1077.615) (<= (cadr lst) 1141.695)) (append (list (car lst)) (list "C#6"))]
      [(and (> (cadr lst) 1141.695) (<= (cadr lst) 1209.585)) (append (list (car lst)) (list "D6"))]
      [(and (> (cadr lst) 1209.585) (<= (cadr lst) 1281.51)) (append (list (car lst)) (list "Eb6"))]
      [(and (> (cadr lst) 1281.51) (<= (cadr lst) 1357.71)) (append (list (car lst)) (list "E6"))]
      [(and (> (cadr lst) 1357.71) (<= (cadr lst) 1447.445)) (append (list (car lst)) (list "F6"))]
      [(and (> (cadr lst) 1447.445) (<= (cadr lst) 1532.98)) (append (list (car lst)) (list "F#6"))]
      [(and (> (cadr lst) 1532.98) (<= (cadr lst) 1614.6)) (append (list (car lst)) (list "G6"))]
      [(and (> (cadr lst) 1614.6) (<= (cadr lst) 1710.61)) (append (list (car lst)) (list "Ab6"))]
      [(and (> (cadr lst) 1710.61) (<= (cadr lst) 1812.33)) (append (list (car lst)) (list "A6"))]
      [(and (> (cadr lst) 1812.33) (<= (cadr lst) 1920.095)) (append (list (car lst)) (list "Bb6"))]
      [(and (> (cadr lst) 1920.095) (<= (cadr lst) 2034.265)) (append (list (car lst)) (list "B6"))]
      [(and (> (cadr lst) 2034.265) (<= (cadr lst) 2155.23)) (append (list (car lst)) (list "C7"))]
      [(and (> (cadr lst) 2155.23) (<= (cadr lst) 2283.39)) (append (list (car lst)) (list "C#7"))]
      [(and (> (cadr lst) 2283.39) (<= (cadr lst) 2419.17)) (append (list (car lst)) (list "D7"))]
      [(and (> (cadr lst) 2419.17) (<= (cadr lst) 2563.02)) (append (list (car lst)) (list "Eb7"))]
      [(and (> (cadr lst) 2563.02) (<= (cadr lst) 2715.425)) (append (list (car lst)) (list "E7"))]
      [(and (> (cadr lst) 2715.425) (<= (cadr lst) 2876.895)) (append (list (car lst)) (list "F7"))]
      [(and (> (cadr lst) 2876.895) (<= (cadr lst) 3047.96)) (append (list (car lst)) (list "F#7"))]
      [(and (> (cadr lst) 3047.96) (<= (cadr lst) 3229.2)) (append (list (car lst)) (list "G7"))]
      [(and (> (cadr lst) 3229.2) (<= (cadr lst) 3421.22)) (append (list (car lst)) (list "Ab7"))]
      [(and (> (cadr lst) 3421.22) (<= (cadr lst) 3624.655)) (append (list (car lst)) (list "A7"))]
      [(and (> (cadr lst) 3624.655) (<= (cadr lst) 3840.19)) (append (list (car lst)) (list "Bb7"))]
      [(and (> (cadr lst) 3840.19) (<= (cadr lst) 4068.54)) (append (list (car lst)) (list "B7"))]
      [(and (> (cadr lst) 4068.54) (<= (cadr lst) 4310.465)) (append (list (car lst)) (list "C8"))]
      [(and (> (cadr lst) 4310.465) (<= (cadr lst) 4566.775)) (append (list (car lst)) (list "C#8"))]
      [(and (> (cadr lst) 4566.775) (<= (cadr lst) 4838.33)) (append (list (car lst)) (list "D8"))]
      [(and (> (cadr lst) 4838.33) (<= (cadr lst) 5126.035)) (append (list (car lst)) (list "Eb8"))]
      [(and (> (cadr lst) 5126.035) (<= (cadr lst) 5430.845)) (append (list (car lst)) (list "E8"))]
      [(and (> (cadr lst) 5430.845) (<= (cadr lst) 5753.78)) (append (list (car lst)) (list "F8"))]
      [(and (> (cadr lst) 5753.78) (<= (cadr lst) 6095.92)) (append (list (car lst)) (list "F#8"))]
      [(and (> (cadr lst) 6095.92) (<= (cadr lst) 6458.405)) (append (list (car lst)) (list "G8"))]
      [(and (> (cadr lst) 6458.405) (<= (cadr lst) 6842.44)) (append (list (car lst)) (list "Ab8"))]
      [(and (> (cadr lst) 6842.44) (<= (cadr lst) 7249.31)) (append (list (car lst)) (list "A8"))]
      [(and (> (cadr lst) 7249.31) (<= (cadr lst) 7680.375)) (append (list (car lst)) (list "Bb8"))]
      [(> (cadr lst) 7680.375) (append (list (car lst)) (list "B8"))]))) 


;;;Procedure
;;;   notes-in-order
;;;Parameters
;;;   lst, a list of lists in form '(timestamp note)
;;;Purpose
;;;   reformat lst, to a form where there aren't duplicate notes, instead there is the note and its duration 
;;;Produces
;;;   newlst, a list of lists in form '(note note-duration)
(define (notes-in-order lst)
  (letrec ([kernel (lambda (lst temp-duration final)
            (cond
              [(<= (length lst) 1) (error "This shouldn't be possible.")]
              [(= 2 (length lst))
                 (if (equal? (cadar lst) (cadr (list-ref lst 1)))
                     (append final (list (list (cadar lst) (/ (truncate (* 100 (+ .01 temp-duration))) 100))))
                     (append final (list
                                         (list (cadar lst) (/ (truncate (* 100 temp-duration)) 100))
                                         (list (cadr (list-ref lst 1)) .01))))]
              [(equal? (cadar lst) (cadr (list-ref lst 1)))
               (kernel (cdr lst) (+ temp-duration 0.01) final)]
              [else
               (kernel (cdr lst) 0.01 (append final (list (list (cadar lst) (/ (truncate (* 100 temp-duration)) 100)))))]))]
           [get-substantial (lambda (lst newlist)
                              (if (null? lst)
                                (reverse newlist)
                                (if (> (cadar lst) .05)
                                  (get-substantial (cdr lst) (cons (car lst) newlist))
                                  (get-substantial (cdr lst) newlist))))])
    (get-substantial (kernel lst .01 null) null))) ; we only want the notes that have substantial duration


;;;Procedure
;;;   analyze-pattern-notes
;;;Parameters
;;;   table, a list of lists in form '(“note” time(number))
;;;   note, a string
;;;Purpose
;;;   returns a list of lists 
;;;   where the first element in each entry is a string of a note that comes after the given note
;;;   and the second element is the number of those notes 
;;;   For example, if you enter (analyze-pattern-notes table "C")
;;;   and the output is '(("F" 1) ("D" 1)) it means there is one “F” and one “D” that comes after “C”
;;;Produces
;;;   newlst, a list of lists in which each entry is the form '(note (total number of note))
(define analyze-pattern-notes
  (lambda (table note)
  (cons note 
(tally-all
     (letrec
         ([kernel
           (lambda (table)
             (cond
               [(null? (cdr table))
                null]
               [(equal? (car (car table)) note)
                    (cons (car (cadr table)) (kernel (cdr table)))]
               [else
                (kernel (cdr table))]))])
       (kernel table))))))

;;;Procedure
;;;   analyze-pattern-beat
;;;Parameters
;;;   table, a list of lists in form '(“note” time(number))
;;;   duration, a number
;;;Purpose
;;;   returns a list of lists 
;;;   where the first element in each entry is a string of a note that comes after the given note
;;;   and the second element is the number of those notes 
;;;   For example, if you enter (analyze-pattern-notes table 0.03)
;;;   and the output is '((0.01 2) (0.02 1)) it means there are two 0.01 and one 0.02 that comes after 0.03
;;;Produces
;;;   newlst, a list of lists in which each entry is the form '(duration(number) (total number of duration))
(define analyze-pattern-beat
  (lambda (table beat)
    (cons beat
(tally-all
     (letrec
         ([kernel2
           (lambda (table)
             (cond
               [(null? (cdr table))
                null]
               [(equal? (car (car table)) beat)
                    (cons (car (cadr table)) (kernel2 (cdr table)))]
               [else
                (kernel2 (cdr table))]))])
       (kernel2 table))))))

(define note-list (list "C0" "C#0" "D0" "Eb0" "E0" "F0" "F#0" "G0" "Ab0" "A0" "Bb0" "B0"
                        "C1" "C#1" "D1" "Eb1" "E1" "F1" "F#1" "G1" "Ab1" "A1" "Bb1" "B1"
                        "C2" "C#2" "D2" "Eb2" "E2" "F2" "F#2" "G2" "Ab2" "A2" "Bb2" "B2"
                        "C3" "C#3" "D3" "Eb3" "E3" "F3" "F#3" "G3" "Ab3" "A3" "Bb3" "B3"
                        "C4" "C#4" "D4" "Eb4" "E4" "F4" "F#4" "G4" "Ab4" "A4" "Bb4" "B4"
                        "C5" "C#5" "D5" "Eb5" "E5" "F5" "F#5" "G5" "Ab5" "A5" "Bb5" "B5"
                        "C6" "C#6" "D6" "Eb6" "E6" "F6" "F#6" "G6" "Ab6" "A6" "Bb6" "B6"
                        "C7" "C#7" "D7" "Eb7" "E7" "F7" "F#7" "G7" "Ab7" "A7" "Bb7" "B7"
                        "C8" "C#8" "D8" "Eb8" "E8" "F8" "F#8" "G8" "Ab8" "A8" "Bb8" "B8" "rest"))


(define durations-list (list "1/16" "1/8" "1/4" "1/2" "3/4" "1" "5/4" "3/2" "7/4" "2"))

;;;Procedure
;;;   clean-beat
;;;Parameters
;;;   file, a file path as a string 
;;;Purpose
;;;   to convert the durations in seconds into durations as musical rhythms
;;;Produces
;;;   result, a list of lists that each contain a single string.
;;;Preconditions
;;;   file must be a valid file location and must be in the correct format
;;;Postconditions
;;;   All strings will be either "1/16", "1/8", "1/4", "1/2", "1", "5/4", "3/2", "7/4", or "2"
;;;   (reduce + (map string->number (map car result))) will give the total number of beats in the song
;;;   (assuming a 4/4 time signature).
(define clean-beat
 (lambda (file)
   (let ([durations->beat
          (lambda (lst)
    (cond
      [(and (> (cadr lst) 0) (<= (cadr lst) .15)) (list "1/16")]
      [(and (> (cadr lst) .15) (<= (cadr lst) .30)) (list "1/8")]
      [(and (> (cadr lst) .3) (<= (cadr lst) .6)) (list "1/4")]
      [(and (> (cadr lst) .6) (<= (cadr lst) 1)) (list "1/2")]
      [(and (> (cadr lst) 1) (<= (cadr lst) 1.4)) (list "3/4")]
      [(and (> (cadr lst) 1.4) (<= (cadr lst) 1.8)) (list "1")]
      [(and (> (cadr lst) 1.8) (<= (cadr lst) 2.2)) (list "5/4")]
      [(and (> (cadr lst) 2.2) (<= (cadr lst) 2.6)) (list "3/2")]
      [(and (> (cadr lst) 2.6) (<= (cadr lst) 3.0)) (list "7/4")]
      [(> (cadr lst) 3)  (list "2")]))])
 (map durations->beat (notes-in-order (clean-notes file))))))

;;;Procedure
;;;   make-note-list
;;;Parameters
;;;   file, a file path as a string 
;;;Purpose
;;;   to return every note possible as well as every note that ever followed the note and how often
;;;   those notes follow the note for a given song.
;;;Produces
;;;   result, a list of lists that will either contain only one string (if the note was never played
;;;   in the song) or a number of other lists each containing a note that followed the first note and
;;;   the number of times that note followed the first note.
;;;Preconditions
;;;   file must be a valid file location and must be in the correct format
;;;Postconditions
;;;   (length result) will always equal 109 because there are 9 octaves of notes with 12 notes each
;;;   plus one rest and 12*9 + 1 equals 109.
;;;   adding the cdr of all of the frequency lists will yield how many times the note that the frequency
;;;   lists correspond to was played in the song. (Unless the note was the last note of the song, in which
;;;   case it would be that - 1.)
(define make-note-list
  (lambda (file)
    (let kernel ([lst-so-far null]
                 [remaining note-list])
      (cond
        [(null? remaining) lst-so-far]
        [else (kernel (append lst-so-far (list (analyze-pattern-notes (notes-in-order (clean-notes file)) (car remaining)))) (cdr remaining))]))))

;;;Procedure
;;;   make-beat-list
;;;Parameters
;;;   file, a file path as a string 
;;;Purpose
;;;   to return every duration or "beat" possible as well as every beat that ever followed the beat and how often
;;;   those beats follow the beat for a given song.
;;;Produces
;;;   result, a list of lists that will either contain only one string (if the beat was never played
;;;   in the song) or a number of other lists each containing a beat that followed the first beat and
;;;   the number of times that beat followed the first beat.
;;;Preconditions
;;;   file must be a valid file location and must be in the correct format
;;;Postconditions
;;;   (length result) will always equal 10 because there are 10 types of beats
;;;   adding the cdr of all of the frequency lists will yield how many times the beat that the frequency
;;;   lists correspond to was played in the song. (Unless the beat was the last beat of the song, in which
;;;   case it would be that - 1.)
(define make-beat-list
  (lambda (file)
    (let kernel ([lst-so-far null]
                 [remaining durations-list])
      (cond
        [(null? remaining) lst-so-far]
        [else (kernel (append lst-so-far
                             (list (analyze-pattern-beat (clean-beat file) (car remaining))))
                      (cdr remaining))]))))

;;;Procedure
;;;   get-song-files
;;;Parameters
;;;   location-path, a str
;;;   l-bound, an integer
;;;   u-bound, an integer
;;;Purpose
;;;   creates a list of song file pathways, based on the entered location-path and a range of preferred songs
;;;   saves time for the user from having to define all the song pathways individually
;;;Produces
;;;   song-files, a list of strings
;;;Preconditions
;;; * the orchset folder must be downloaded on your computer (included in submission folder)
;;; * IMPORTANT: location-path = the location pathway up to where the GT folder within Orchset is saved on your computer
;;;      (for example, "C:/Users/cahol/Desktop/Orchset/GT/" is a valid answer)
;;; * l-bound is the lower index and u-bound is the high index of the range of songs you want
;;;    to format into a list (in the order that the songs are listed in the Orchset file)
;;;  * l-bound must be in the following range: 0 <= l-bound <= 62
;;;  * u-bound must be in the following range: 1 <= u-bound <= 63
;;;  * l-bound must be < u-bound (they can't be equal!)
;;;  * Examples of entries of l-bound and u-bound:
;;;        *  l-bound = 0 and u-bound = 63 ... List of all songs
;;;        *  l-bound = 0 and u-bound = 12 ... List of the first 13 songs listed in the orchset folder (The 13 Beethoven songs)
;;;        *  etc. (various ranges can select particular composer's songs)
;;;Postconditions
;;;  * song-files = a list of song file pathways, which were generated by string-appending location-path
;;;                 to each of the songs in the range
;;;  * (length song-files) = (u-bound - l-bound) + 1
;;;  * elements in song-files will be complete location pathways to various songs in the GT folder within the Orchset folder
(define get-song-files
  (lambda (location-path l-bound u-bound)
    (letrec ([song-strings (list "Beethoven-S3-I-ex1.mel" "Beethoven-S3-I-ex2.mel" "Beethoven-S3-I-ex3.mel"
                          "Beethoven-S3-I-ex5.mel" "Beethoven-S3-I-ex6.mel" "Beethoven-S5-I-ex1.mel"
                          "Beethoven-S5-II-ex1.mel" "Beethoven-S5-II-ex2.mel" "Beethoven-S5-II-ex3.mel"
                          "Beethoven-S7-II-ex2.mel" "Beethoven-S9-II-ex1.mel" "Beethoven-S9-II-ex2.mel"
                          "Beethoven-S9-II-ex3.mel" "Brahms-HungarianDance-n5-ex1.mel" "Brahms-S3-III-ex1.mel"
                          "Brahms-S3-III-ex2.mel" "Brahms-S3-III-ex3.mel" "Dvorak-S9-IV-ex1.mel"
                          "Dvorak-S9-IV-ex3.mel" "Dvorak-S9-IV-ex4.mel" "Dvorak-S9-IV-ex5.mel"
                          "Grieg-PeerGynt-HallMountainKing-ex1.mel" "Grieg-PeerGynt-MorningMood-ex1.mel" "Grieg-PeerGynt-MorningMood-ex2.mel"
                          "Haydn-S94-Andante-ex2.mel" "Haydn-S94-Menuet-ex1.mel" "Haydn-S94-Menuet-ex2.mel"
                          "Holst-ThePlanets-Jupiter-ex1.mel" "Holst-ThePlanets-Jupiter-ex2.mel" "Holst-ThePlanets-Jupiter-ex3.mel"
                          "Holst-ThePlanets-Jupiter-ex4.mel" "Musorgski-Ravel-PicturesExhibition-ex4.mel" "Musorgski-Ravel-PicturesExhibition-ex5.mel"
                          "Musorgski-Ravel-PicturesExhibition-ex6.mel" "Musorgski-Ravel-PicturesExhibition-ex7.mel" "Musorgski-Ravel-PicturesExhibition-ex8.mel"
                          "Musorgski-Ravel-PicturesExhibition-ex10.mel" "Musorgski-Ravel-PicturesExhibition-ex11.mel" "Musorgski-Ravel-PicturesExhibition-Promenade1-ex1.mel"
                          "Musorgski-Ravel-PicturesExhibition-Promenade1-ex2.mel" "Profofiev-Romeo&Juliet-DanceKnights-ex1.mel" "Profofiev-Romeo&Juliet-DanceKnights-ex2.mel"
                          "Ravel-Bolero-ex1.mel" "Ravel-Bolero-ex2.mel" "Ravel-Bolero-ex3.mel"
                          "Rimski-Korsakov-Scheherazade-Kalender-ex1.mel" "Rimski-Korsakov-Scheherazade-Kalender-ex2.mel" "Rimski-Korsakov-Scheherazade-Kalender-ex3.mel"
                          "Rimski-Korsakov-Scheherazade-Sea-SinbadShip-ex1.mel" "Rimski-Korsakov-Scheherazade-Sea-SinbadShip-ex2.mel" "Rimski-Korsakov-Scheherazade-Sea-SinbadShip-ex5.mel"
                          "Rimski-Korsakov-Scheherazade-YoungPrincePrincess-ex1.mel" "Rimski-Korsakov-Scheherazade-YoungPrincePrincess-ex2.mel" "Rimski-Korsakov-Scheherazade-YoungPrincePrincess-ex3.mel"
                          "Rimski-Korsakov-Scheherazade-YoungPrincePrincess-ex4.mel" "Schubert-S8-II-ex2.mel" "Smetana-MaVlast-Vltava-ex1.mel"
                          "Smetana-MaVlast-Vltava-ex4.mel" "Strauss-BlueDanube-ex1.mel" "Strauss-BlueDanube-ex2.mel"
                          "Strauss-BlueDanube-ex3.mel" "Tchaikovsky-SwanLake-Scene-ex1.mel" "Tchaikovsky-SwanLake-Scene-ex2.mel"
                          "Wagner-Tannhauser-Act2-ex2.mel")]
             [sublist (take (drop song-strings l-bound) (- (+ 1 u-bound) l-bound))] 
             [kernel (lambda (lst song-list)
                       (if (null? lst)
                           song-list
                           (kernel (cdr lst) (cons (string-append location-path (car lst)) song-list))))])
      (reverse (kernel sublist null)))))
                         
;;;Procedure
;;;   make-note-dataset
;;;Parameters
;;;   songs-input, a list of strings
;;;Purpose
;;;   makes a dataset of all notes, and the notes that follow each note across all songs
;;;Produces
;;;   note-dataset, a list of lists
;;;Preconditions
;;;   * songs-input must be a list of strings, each string a path location to a song in the "GT" folder in Orchset
;;;Postconditions
;;;   * note-dataset = a list of lists, each nested list contains first a note as a string, say "leadnote" (e.g. "C4", "Eb5", etc.)
;;;                    and then possibly a sequence of 2-element lists corresponding to a note and number of occurrences
;;;                    of that note that follow "leadnote" in one of the songs.
;;;   * if a note, note1, is never played in a song in songs-input (or if the only occurrence of note1 is the last note in the melody) then
;;;     (assoc note1 (make-note-dataset songs-input)) will return just the list '(note1) (i.e. the dataset is showing that no notes follow note1                       
(define make-note-dataset
  (lambda (songs-input)
    (letrec ([make-list-elements (lambda (lst listlist)
                       (if (null? lst)
                           listlist
                           (make-list-elements (cdr lst) (append listlist (list (list (car lst)))))))]
             [kernel (lambda (songlist note-database)
                       (if (null? songlist)
                            note-database
                            (kernel (cdr songlist) (map append note-database (map cdr (make-note-list (car songlist)))))))])  
      (kernel songs-input (make-list-elements note-list null)))))


;;;Procedure
;;;   make-beat-dataset
;;;Parameters
;;;   songs-input, a list of strings
;;;Purpose
;;;   makes a dataset of all beats (note durations), and the beats that follow each note across all songs
;;;Produces
;;;   beat-dataset, a list of lists
;;;Preconditions
;;;   * songs-input must be a list of strings, each string a path location to a song in the "GT" folder in Orchset
;;;Postconditions
;;;   * beat-dataset = a list of lists, each nested list contains first a beat as a string, say "leadbeat" (e.g. "1/4", "1/16" etc.)
;;;                    and then possibly a sequence of 2-element lists correpsonding to a beat and number of occurances
;;;                    of that beat that follow "leadnote" in one of the songs.
;;;   * if a beat, beat1, is never played in a song in songs-input (or if the only occurance of beat1 is the last note in the melody) then
;;;     (assoc beat1 (make-beat-dataset songs-input)) will return just the list '(beat1) (i.e. the dataset is showing that no beats follow beat1
(define make-beat-dataset
  (lambda (songs-input)
    (letrec ([make-list-elements (lambda (lst listlist)
                       (if (null? lst)
                           listlist
                           (make-list-elements (cdr lst) (append listlist (list (list (car lst)))))))]
             [kernel (lambda (songlist beat-database)
                       (if (null? songlist)
                            beat-database
                            (kernel (cdr songlist) (map append beat-database (map cdr (make-beat-list (car songlist)))))))])  
      (kernel songs-input (make-list-elements durations-list null)))))

;;; Procedure:
;;;  generate-note-or-beat
;;; Parameters:
;;;  str, a string
;;;  song-list, a list of tables
;;; Purpose:
;;;  to generate a new note or beat based on the probability established by the dataset
;;; Produces:
;;;  result, a string
;;; Preconditions:
;;;  [no additionals]
;;; Postconditions:
;;;  If the strings inside song-list are strings from note-list, then result would also be one of the strings from note-list.
;;;  If the strings inside song-list are from durations-list, then result would be a string from durations-list.
(define generate-note-or-beat
  (lambda (str song-list)
    (if (null? (cdr (assoc str song-list)))
        (let* ([filtered-list (filter (o (section < 1 <>) length) song-list)]
               [randomnote (car (list-ref filtered-list (random (length filtered-list))))]
               [lst-length (reduce + (map cadr (cdr (assoc randomnote song-list))))]
               [randomnum (+ (random lst-length) 1)])
          (letrec ([kernel
                     (lambda (so-far lst)
                       (cond
                         [(<= (- so-far (cadr (car lst))) 0)
                          (car (car lst))]
                         [else
                          (kernel (- so-far (cadr (car lst))) (cdr lst))]))])
             (kernel randomnum (cdr (assoc randomnote song-list)))))
    (let* ([lst-length (reduce + (map cadr (cdr (assoc str song-list))))]
           [randomnum (+ (random lst-length) 1)])
           (letrec ([kernel
                     (lambda (so-far lst)
                       (cond
                         [(<= (- so-far (cadr (car lst))) 0)
                          (car (car lst))]
                         [else
                          (kernel (- so-far (cadr (car lst))) (cdr lst))]))])
             (kernel randomnum (cdr (assoc str song-list))))))))

;;; Procedure:
;;;  make-song
;;; Parameters:
;;;  song-length, a positive integer
;;;   location-path, a str
;;;   l-bound, an integer
;;;   u-bound, an integer
;;; Purpose:
;;;  to generate a song based on a range of songs
;;; Produces:
;;;  output-song, a list in which each entry is a list of two strings
;;; Preconditions:
;;; * the orchset folder must be downloaded on your computer (included in submission folder)
;;; * IMPORTANT: location-path = the location pathway up to where the GT folder within Orchset is saved on your computer
;;;      (for example, "C:/Users/cahol/Desktop/Orchset/GT/" is a valid answer)
;;; * l-bound is the lower index and u-bound is the high index of the range of songs you want
;;;    to format into a list (in the order that the songs are listed in the Orchset file)
;;;  * l-bound must be in the following range: 0 <= l-bound <= 62
;;;  * u-bound must be in the following range: 1 <= u-bound <= 63
;;;  * l-bound must be < u-bound (they can't be equal!)
;;;  * Examples of entries of l-bound and u-bound:
;;;        *  l-bound = 0 and u-bound = 63 ... List of all songs
;;;        *  l-bound = 0 and u-bound = 12 ... List of the first 13 songs listed in the orchset folder (The 13 Beethoven songs)
;;;        *  etc. (various ranges can select particular composer's songs)
;;; Postconditions:
;;;   * output-song = a list of 2-entry lists, each 2-entry list takes the form ‘(note duration)
;;;   * the note and note duration in (car output-song) is randomly selected from the notes
;;;     and note durations that occur in the range of songs inputted 
;;;   * each note and note duration in (cdr output-song) was produced based on the previous
;;;     note and note duration in output-song
(define make-song
  (λ (song-length location-path u-bound l-bound)
    (letrec ([song-list (get-song-files location-path u-bound l-bound)]
             [note-data (make-note-dataset song-list)]
             [beat-data (make-beat-dataset song-list)]
             [filtered-song-list (filter (o (section < 1 <>) length) note-data)]
             [filtered-beat-list (filter (o (section < 1 <>) length) beat-data)]
             [starting-note (car (list-ref filtered-song-list (random (length filtered-song-list))))]
             [starting-beat (car (list-ref filtered-beat-list (random (length filtered-beat-list))))]
             [generate-new-note
              (lambda (prev-note)
                (generate-note-or-beat prev-note note-data))]
             [generate-new-beat
              (lambda (prev-beat)
                (generate-note-or-beat prev-beat beat-data))]
             [compose-notes (λ (note song-accumulation song-length)
                              (let ([newnote (list (generate-note-or-beat (car note) note-data) (generate-note-or-beat (cadr note) beat-data))])
                                (if (and (not (null? song-accumulation))
                                       (< song-length (reduce + (map string->number (map cadr song-accumulation)))))
                                    song-accumulation
                                    (compose-notes newnote (append (list newnote) song-accumulation) song-length))))])           
      (compose-notes (list starting-note starting-beat) null song-length))))
