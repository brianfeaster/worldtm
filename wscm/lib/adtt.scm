(load "lib/adt.scm")
(define pass #t)

(define myList (ListCreate))
(displayl "\nmyList=" myList)

(displayl "\nListGet=" (ListGet myList))

(displayl "\nListAdd=" (ListAdd myList 'brian))
(displayl "\nmyList=" myList)
(displayl "\nListGet=" (ListGet myList))

(displayl "\nListAdd=" (ListAdd myList 'emily))
(displayl "\nmyList=" myList)
(displayl "\nListGet=" (ListGet myList))

(displayl "\nListAdd=" (ListAdd myList 'doug))
(displayl "\nmyList=" myList)
(displayl "\nListGet=" (ListGet myList))

(displayl "\nListDel emily=" (ListDel myList 'emily))
(displayl "\nmyList=" myList)
(displayl "\nListGet=" (ListGet myList))

(displayl "\nListDelFn brian=" (ListDelFn myList (lambda (o) (equal? o 'brian))))
(displayl "\nmyList=" myList)
(displayl "\nListGet=" (ListGet myList))

(newline)
(set! pass (and pass (equal? (ListGet myList) '(doug))))

(define bl (BListCreate 1 2 3))
(BListAddBack bl 'c)
(BListAddFront bl 'b)
(BListAddFront bl 'a)

(displayl (BListList bl) " list\r\n" (BListReverseList bl) " reverse list\r\n")

(set! pass (and pass 
   (equal? (BListList bl) '(a b 1 2 3 c))
   (equal? (BListReverseList bl) '(c 3 2 1 b a))))

(displayl "\r\nfont=" (BListDelFront bl) "  back="(BListDelBack bl) " empty=" (BListEmpty? bl))
(displayl "\r\nlist="(BListList bl) " list_reverse=" (BListReverseList bl))

(displayl "\r\nfont=" (BListDelFront bl) "  back="(BListDelBack bl) " empty=" (BListEmpty? bl))
(displayl "\r\nlist="(BListList bl) " list_reverse=" (BListReverseList bl))

(BListAddFront bl 'x)
(BListAddFront bl 'y)
(BListAddFront bl 'z)
(displayl "\r\nlist="(BListList bl) " list_reverse=" (BListReverseList bl))

(set! pass (and pass (equal? (BListGetFront bl) 'z) (equal? (BListGetBack bl) 2)))
(newline)

(if pass (begin (display 'PASS) #t)
         (begin (display 'FAIL) #f))
