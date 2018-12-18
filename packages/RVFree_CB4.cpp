//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("RVFree_CB4.res");
USEPACKAGE("vcl40.bpi");
USEUNIT("RVStyle.pas");
USEUNIT("RichView.pas");
USEUNIT("RVFreeReg.pas");
USERES("RVFreeReg.dcr");
USEUNIT("RVScroll.pas");
USEFORMNS("RVSEdit.pas", Rvsedit, frmRVSEdit);
USEUNIT("PtblRV.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
