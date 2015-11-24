;;;

(asdf:operate 'asdf:load-op 'wxcl)
(use-package "FFI")
(ffi:default-foreign-language :stdc)


;;; begin wxGlade: dependencies
(use-package :wxCL)
(use-package :wxColour)
(use-package :wxEvent)
(use-package :wxEvtHandler)
(use-package :wxFrame)
(use-package :wxWindow)
(use-package :wx_main)
(use-package :wx_wrapper)
;;; end wxGlade

;;; begin wxGlade: extracode
;;; end wxGlade


(defclass StockAction()
        ((top-window :initform nil :accessor slot-top-window)))

(defun make-StockAction ()
        (let ((obj (make-instance 'StockAction)))
          (init obj)
          (set-properties obj)
          (do-layout obj)
          obj))

(defmethod init ((obj StockAction))
"Method creates the objects contained in the class."
        ;;; begin wxGlade: StockAction.__init__
        (setf (slot-top-window obj) (wxFrame_create nil wxID_ANY "" -1 -1 -1 -1 (logior wxDEFAULT_FRAME_STYLE wxTAB_TRAVERSAL)))
        ;;; end wxGlade
        )

(defmethod set-properties ((obj StockAction))
        ;;; begin wxGlade: StockAction.__set_properties
        (wxFrame_SetTitle (slot-top-window obj) (_"Stock Action"))
        (slot-top-window obj).wxWindow_SetSize((150,150))
        ;;; end wxGlade
        )

(defmethod do-layout ((obj StockAction))
        ;;; begin wxGlade: StockAction.__do_layout
        (wxFrame_layout (slot-StockAction self))
        ;;; end wxGlade
        )

;;; end of class StockAction


