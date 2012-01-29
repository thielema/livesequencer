-- | might be moved to wx package
module Utility.WX where

import qualified Graphics.UI.WX as WX
import Graphics.UI.WX.Controls ( TextCtrl, Notebook )

-- import qualified Graphics.UI.WXCore as WXCore
-- import qualified Graphics.UI.WXCore.WxcClassesAL as WXCAL
import qualified Graphics.UI.WXCore.WxcClassesMZ as WXCMZ

import Control.Functor.HT ( void )


cursor :: WX.Attr (TextCtrl a) Int
cursor =
    WX.newAttr "cursor"
        WXCMZ.textCtrlGetInsertionPoint
        WXCMZ.textCtrlSetInsertionPoint

editable :: WX.Attr (TextCtrl a) Bool
editable =
    WX.newAttr "editable"
        WXCMZ.textCtrlIsEditable
        WXCMZ.textCtrlSetEditable

modified :: WX.ReadAttr (TextCtrl a) Bool
modified =
    WX.readAttr "modified"
        WXCMZ.textCtrlIsModified
--        WXCMZ.textCtrlDiscardEdits
--        WXCMZ.textCtrlMarkDirty

notebookSelection :: WX.Attr (Notebook a) Int
notebookSelection =
    WX.newAttr "selection"
        WXCMZ.notebookGetSelection
        (\nb -> void . WXCMZ.notebookSetSelection nb)
