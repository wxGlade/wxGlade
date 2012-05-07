#!/usr/bin/env lisp
;;;
;;; generated by wxGlade "faked test version"
;;;

(asdf:operate 'asdf:load-op 'wxcl)
(use-package "FFI")
(ffi:default-foreign-language :stdc)

;;; begin wxGlade: dependencies
(use-package :wxBitmapButton)
(use-package :wxCL)
(use-package :wxColour)
(use-package :wxEvent)
(use-package :wxEvtHandler)
(use-package :wxFrame)
(use-package :wxSizer)
(use-package :wxWindow)
(use-package :wx_main)
(use-package :wx_wrapper)
;;; end wxGlade

;;; begin wxGlade: extracode
;;; end wxGlade


(defclass MyFrame()
        ((top-window :initform nil :accessor slot-top-window)
        (bitmap-button-1 :initform nil :accessor slot-bitmap-button-1)
        (sizer-1 :initform nil :accessor slot-sizer-1)))

(defun make-MyFrame ()
        (let ((obj (make-instance 'MyFrame)))
          (init obj)
          (set-properties obj)
          (do-layout obj)
          obj))

(defmethod init ((obj MyFrame))
"Method creates the objects contained in the class."
        ;;; begin wxGlade: MyFrame.__init__
        (setf (slot-top-window obj) (wxFrame_create nil wxID_ANY "" -1 -1 -1 -1 wxDEFAULT_FRAME_STYLE))
        (setf (slot-bitmap-button-1 obj) (wxBitmapButton_Create (slot-top-window obj) wxID_ANY (wxBitmap_CreateLoad "icons/wxglade_small.png", wxBITMAP_TYPE_ANY) -1 -1 -1 -1 0))
        )
        ;;; end wxGlade

(defmethod set-properties ((obj MyFrame))
        ;;; begin wxGlade: MyFrame.__set_properties
        (wxFrame_SetTitle (slot-top-window obj) "frame_1")
        (wxButton_SetDefault (slot-bitmap-button-1 obj))        )
        ;;; end wxGlade

(defmethod do-layout ((obj MyFrame))
        ;;; begin wxGlade: MyFrame.__do_layout
        (setf (slot-sizer-1 obj) (wxBoxSizer_Create  wxVERTICAL))
        (wxSizer_AddWindow (slot-sizer-1 obj) (slot-bitmap-button-1 obj) 0 0 0 nil)
        (wxWindow_SetSizer (slot-top-window obj) (slot-sizer-1 obj))
        (wxSizer_Fit (slot-sizer-1 obj) (slot-top-window obj))
        (wxFrame_layout (slot-frame-1 slef))
        )
        ;;; end wxGlade

;;; end of class MyFrame


(defun init-func (fun data evt)
    (let ((frame-1 (make-MyFrame)))
    (ELJApp_SetTopWindow (slot-top-window frame-1))
    (wxWindow_Show (slot-top-window frame-1))))

(unwind-protect
    (Eljapp_initializeC (wxclosure_Create #'init-func nil) 0 nil)
    (ffi:close-foreign-library "../miscellaneous/wxc-msw2.6.2.dll"))
