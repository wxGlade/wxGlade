#!/usr/bin/env lisp
;;;
;;; generated by wxGlade "faked test version"
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
(use-package :wxMenu)
(use-package :wxMenuBar)
(use-package :wxSizer)
(use-package :wxTextCtrl)
(use-package :wxToolBar)
(use-package :wxWindow)
(use-package :wx_main)
(use-package :wx_wrapper)
;;; end wxGlade

;;; begin wxGlade: extracode
;;; end wxGlade


(defclass Frame186()
        ((top-window :initform nil :accessor slot-top-window)
        (Bug186-Frame-menubar :initform nil :accessor slot-Bug186-Frame-menubar)
        (Bug186-Frame-toolbar :initform nil :accessor slot-Bug186-Frame-toolbar)
        (text-ctrl-1 :initform nil :accessor slot-text-ctrl-1)
        (text-ctrl-2 :initform nil :accessor slot-text-ctrl-2)
        (text-ctrl-3 :initform nil :accessor slot-text-ctrl-3)
        (text-ctrl-4 :initform nil :accessor slot-text-ctrl-4)
        (sizer-2 :initform nil :accessor slot-sizer-2)
        (sizer-1 :initform nil :accessor slot-sizer-1)))

(defun make-Frame186 ()
        (let ((obj (make-instance 'Frame186)))
          (init obj)
          (set-properties obj)
          (do-layout obj)
          obj))

(defmethod init ((obj Frame186))
"Method creates the objects contained in the class."
        ;;; begin wxGlade: Frame186.__init__
        
        ;;; Menu Bar
        (setf (slot-Bug186-Frame-menubar obj) (wxMenuBar_Create 0))
        global myMagicMenu; myMagicMenu = wxNewId()
        (let ((File (wxMenu_Create "" 0))))
        (wxMenu_Append File myMagicMenu (_"Magic") "" wxITEM_NORMAL)
        (wxMenuBar_Append (slot-Bug186-Frame-menubar obj) File (_"File")))
        (wxFrame_SetMenuBar (slot-top-window obj) (slot-Bug186-Frame-menubar obj))
        ;;; Menu Bar end
        
	;;; Tool Bar
        (setf (slot-Bug186-Frame-toolbar obj) (wxToolBar_Create (slot-top-window obj) -1 -1 -1 -1 -1 wxTB_HORIZONTAL))
        (wxFrame_SetToolBar (slot-top-window obj) (slot-Bug186-Frame-toolbar obj))
        global myMagicTool; myMagicTool = wxNewId()
        (wxToolBar_AddTool (slot-Bug186-Frame-toolbar obj) myMagicTool (_"Magic") wxBitmap_Create(32 32) wxNullBitmap wxITEM_NORMAL (_"Do a MAGIC action") (_"It's really MAGIC"))
        ;;; Tool Bar end
        (setf (slot-text-ctrl-1 obj) (wxTextCtrl_Create (slot-top-window obj) wxID_ANY (_"Id: automatic (default behaviour)") -1 -1 -1 -1 0))
        (setf (slot-text-ctrl-2 obj) (wxTextCtrl_Create (slot-top-window obj) 12123 (_"Id: numeric value \"12123\"") -1 -1 -1 -1 0))
        (setf (slot-text-ctrl-3 obj) (wxTextCtrl_Create (slot-top-window obj) wxID_ANY (_"Id: predefined identify: \"wxID_ANY\"") -1 -1 -1 -1 0))
        global myButtonId; myButtonId = wxNewId()
        (setf (slot-text-ctrl-4 obj) (wxTextCtrl_Create (slot-top-window obj) myButtonId (_"Id: variable assignment \"myButtonId=?\"") -1 -1 -1 -1 0))
        ;;; end wxGlade
        )

(defmethod set-properties ((obj Frame186))
        ;;; begin wxGlade: Frame186.__set_properties
        (wxFrame_SetTitle (slot-top-window obj) (_"frame_1"))
        (slot-top-window obj).wxWindow_SetSize((300, 300))
        (wxToolBar_Realize (slot-Bug186-Frame-toolbar obj))
        ;;; end wxGlade
        )

(defmethod do-layout ((obj Frame186))
        ;;; begin wxGlade: Frame186.__do_layout
        (setf (slot-sizer-1 obj) (wxBoxSizer_Create wxVERTICAL))
        (setf (slot-sizer-2 obj) (wxBoxSizer_Create wxVERTICAL))
        (wxSizer_AddWindow (slot-sizer-2 obj) (slot-text-ctrl-1 obj) 1 (logior wxALL wxEXPAND) 5 nil)
        (wxSizer_AddWindow (slot-sizer-2 obj) (slot-text-ctrl-2 obj) 1 (logior wxALL wxEXPAND) 5 nil)
        (wxSizer_AddWindow (slot-sizer-2 obj) (slot-text-ctrl-3 obj) 1 (logior wxALL wxEXPAND) 5 nil)
        (wxSizer_AddWindow (slot-sizer-2 obj) (slot-text-ctrl-4 obj) 1 (logior wxALL wxEXPAND) 5 nil)
        (wxSizer_AddSizer (slot-sizer-1 obj) (slot-sizer-2 obj) 1 wxEXPAND 0 nil)
        (wxWindow_SetSizer (slot-top-window obj) (slot-sizer-1 obj))
        (wxFrame_layout (slot-Bug186-Frame self))
        ;;; end wxGlade
        )

;;; end of class Frame186


(defun init-func (fun data evt)
        (let ((Bug186-Frame (make-Frame186)))
        (ELJApp_SetTopWindow (slot-top-window Bug186-Frame))
        (wxWindow_Show (slot-top-window Bug186-Frame))))
;;; end of class MyApp

    (setf (textdomain) "app") ;; replace with the appropriate catalog name
    (defun _ (msgid) (gettext msgid "app"))


(unwind-protect
    (Eljapp_initializeC (wxclosure_Create #'init-func nil) 0 nil)
    (ffi:close-foreign-library "../miscellaneous/wxc-msw2.6.2.dll"))
