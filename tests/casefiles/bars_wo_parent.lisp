#!/usr/bin/env lisp
;;;
;;; generated by wxGlade 383ee447e2c1+ on Thu May 26 07:03:27 2016
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
(use-package :wxSizer)
(use-package :wxStaticText)
(use-package :wxWindow)
(use-package :wx_main)
(use-package :wx_wrapper)
;;; end wxGlade

;;; begin wxGlade: extracode
;;; end wxGlade


(defclass MyMenuBar()
        ((top-window :initform nil :accessor slot-top-window)))

(defun make-MyMenuBar ()
        (let ((obj (make-instance 'MyMenuBar)))
          (init obj)
          (set-properties obj)
          (do-layout obj)
          obj))

(defmethod init ((obj MyMenuBar))
"Method creates the objects contained in the class."
        ;;; begin wxGlade: MyMenuBar.__init__
        (let ((wxglade_tmp_menu (wxMenu_Create "" 0)))
        		(wxMenuBar_Append (slot-menubar-1 obj) wxglade_tmp_menu "File"))
        ;;; end wxGlade
        )

(defmethod set-properties ((obj MyMenuBar))
        ;;; begin wxGlade: MyMenuBar.__set_properties
        pass
        ;;; end wxGlade
        )

(defmethod do-layout ((obj MyMenuBar))
        ;;; begin wxGlade: MyMenuBar.__do_layout
        pass
        ;;; end wxGlade
        )

;;; end of class MyMenuBar



(defclass MyToolBar()
        ((top-window :initform nil :accessor slot-top-window)))

(defun make-MyToolBar ()
        (let ((obj (make-instance 'MyToolBar)))
          (init obj)
          (set-properties obj)
          (do-layout obj)
          obj))

(defmethod init ((obj MyToolBar))
"Method creates the objects contained in the class."
        ;;; begin wxGlade: MyToolBar.__init__
        ;;; end wxGlade
        )

(defmethod set-properties ((obj MyToolBar))
        ;;; begin wxGlade: MyToolBar.__set_properties
        (wxToolBar_Realize (slot-toolbar-1 obj))
        ;;; end wxGlade
        )

(defmethod do-layout ((obj MyToolBar))
        ;;; begin wxGlade: MyToolBar.__do_layout
        pass
        ;;; end wxGlade
        )

;;; end of class MyToolBar



(defclass MyFrame()
        ((top-window :initform nil :accessor slot-top-window)
        (label-1 :initform nil :accessor slot-label-1)
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
        (setf (slot-label-1 obj) (wxStaticText_Create (slot-top-window obj) wxID_ANY "placeholder - every design\nneeds a toplevel window" -1 -1 -1 -1 wxALIGN_CENTER))
        ;;; end wxGlade
        )

(defmethod set-properties ((obj MyFrame))
        ;;; begin wxGlade: MyFrame.__set_properties
        (wxFrame_SetTitle (slot-top-window obj) "frame_1")
        ;;; end wxGlade
        )

(defmethod do-layout ((obj MyFrame))
        ;;; begin wxGlade: MyFrame.__do_layout
        (setf (slot-sizer-1 obj) (wxBoxSizer_Create wxVERTICAL))
        (wxSizer_AddWindow (slot-sizer-1 obj) (slot-label-1 obj) 1 (logior wxALIGN_CENTER wxALL wxEXPAND) 0 nil)
        (wxWindow_SetSizer (slot-top-window obj) (slot-sizer-1 obj))
        (wxFrame_layout (slot-frame-1 self))
        (slot-top-window obj).wxWindow_SetSize((200, 200))
        ;;; end wxGlade
        )

;;; end of class MyFrame


(defun init-func (fun data evt)
        (let ((frame-1 (make-MyFrame)))
        (ELJApp_SetTopWindow (slot-top-window frame-1))
        (wxWindow_Show (slot-top-window frame-1))))
;;; end of class MyApp


(unwind-protect
    (Eljapp_initializeC (wxclosure_Create #'init-func nil) 0 nil)
    (ffi:close-foreign-library "../miscellaneous/wxc-msw2.6.2.dll"))
