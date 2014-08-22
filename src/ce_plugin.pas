unit ce_plugin;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, dynlibs;

type

  TCEHost = type Pointer;
  TCEPlugin = type Pointer;
const

  // API version
  CE_PLG_API_VER = 0;

// opcodes constants -------------------------------------------------------------]

  HELLO_PLUGIN    = $FFFFFFFF;  // hello world

// Denotes the emiter and the message kind -------------------------------------

  /// Coedit sends an event.
  HOST_EVENT      = $10000000;
  /// Coedit sends some data.
  HOST_DATA       = $20000000;
  /// The plug-in sends an event.
  PLUG_EVENT      = $30000000;
  /// The plug-in sends some data.
  PLUG_DATA       = $40000000;

// Denotes the message context -------------------------------------------------

  /// the dispatcher call is related to the project(s)
  CTXT_PROJ  = $01000000;
  /// the dispatcher call is related to the document(s)
  CTXT_DOCS  = $02000000;
  /// the dispatcher call is related to the edition of a document.
  CTXT_EDIT  = $03000000;
  /// the dispatcher call is related to the Coedit 'Message Widget'.
  CTXT_MSGS  = $04000000;
  /// the dispatcher call is related to the Coedit dialogs.
  CTXT_DLGS  = $05000000;

// The events kinds ------------------------------------------------------------

  /// somethings's just changed.
  EV_CHANGED   = $00000001;
  /// something's just been selected.
  EV_FOCUSED   = $00000002;
  /// something will be closed.
  EV_CLOSE     = $00000003;
  /// something's just been created.
  EV_NEW       = $00000004;
  /// something gonna be compiled.
  EV_COMPILE   = $00000005;
  /// something gonna be executed.
  EV_RUN       = $00000006;

// The data kinds --------------------------------------------------------------

  /// data1 is used to set/get a filename. data1 is a PChar. data0 represents an index.
  DT_FNAME     = $00000001;
  /// data0 represents a count.
  DT_COUNT     = $00000002;

  /// data1 is used to set a message. data1 is a PChar.
  DT_ERR         = $00000001;
  DT_INF         = $00000002;
  DT_WARN        = $00000003;

  DATA_FNAME      = $00000001;

// terminal opCodes (emiter + context + event/data kind) -----------------------

  /// Coedit says that the project's just been modified.
  HOST_PROJ_CHANGED = HOST_EVENT + CTXT_PROJ + EV_CHANGED;

  /// opCode for asking for a document filename. data0 must be the document index.
  PLUG_WANT_DOC_NAME  = PLUG_EVENT + CTXT_DOCS + DT_FNAME;

  /// opCode for getting a document filenmae. data1 is a PChar to the filename.
  HOST_GIVE_DOC_NAME  = HOST_DATA + CTXT_DOCS + DT_FNAME;

  /// opCodes for displaying a message in a dialog box.
  PLUG_DLGS_ERR    = PLUG_DATA + CTXT_DLGS + DT_ERR;
  /// ditto.
  PLUG_DLGS_WARN   = PLUG_DATA + CTXT_DLGS + DT_WARN;
  /// ditto.
  PLUG_DLGS_INF    = PLUG_DATA + CTXT_DLGS + DT_INF;

  /// opCodes for displaying a message in the 'Message Widget'.
  PLUG_MSGS_ERR    = PLUG_DATA + CTXT_MSGS + DT_ERR;
  /// ditto.
  PLUG_MSGS_WARN   = PLUG_DATA + CTXT_MSGS + DT_WARN;
  /// ditto.
  PLUG_MSGS_INF    = PLUG_DATA + CTXT_MSGS + DT_INF;

// host-side prototypes --------------------------------------------------------
type

  (**
   * A plugin asks for some information or it passes data here.
   * Data1 and data2 are some pointers to a particular variable type.
   * In the plugin hostCreatePlugProc, a the location of COedit dispatcher is passed
   * to the plugin.
   *)
  TPlugDispatchToHost = procedure(aPlugin: TCEPlugin; opCode: LongWord; data0: Integer; data1, data2: Pointer); cdecl;

// plugin-side prototypes ------------------------------------------------------

  (**
   * Coedit initializes a plugin, the result is passed during the runtime as "aTarget".
   * In the plugin implementation, it must be named 'createPlug'.
   * If the result is null then the Plugin is not used at all.
   * If the plugin is not warped in a class than the result must be set on something
   * that can be pointed to (e.g: a global variable).
   *)
  THostCreatePlug     = function(aHost: TPlugDispatchToHost): TCEPlugin; cdecl;

  (**
   * Coedit closes and aTarget can be destroyed.
   * In the plugin implementation, it must be named 'destroyPlug'.
   *)
  THostDestroyPlug    = procedure(aTarget: TCEPlugin); cdecl;

  (**
   * Coedit events and data are passed here. data1 and data2 can be casted according to opCode.
   * In the plugin implementation, it must be named 'dispatchToPlug'.
   *)
  THostDispatchToPlug = procedure(aTarget: TCEPlugin; opCode: LongWord; data0: Integer; data1, data2: Pointer); cdecl;



// internal --------------------------------------------------------------------

  PPlugDescriptor = ^TPlugDescriptor;
  TPlugDescriptor = record
    Handle: TLibHandle;
    Plugin: TCEPlugin;
    HostCreatePlug: THostCreatePlug;
    HostDestroyPlug: THostDestroyPlug;
    HostDispatchToPlug: THostDispatchToPlug;
  end;

  TCEPlugDescriptorList = class(TList)
  private
    function getPlugin(index: integer): TPlugDescriptor;
  public
    procedure addPlugin(aValue: PPlugDescriptor);
    property plugin[index: integer]: TPlugDescriptor read getPlugin;
  end;
  TPlugDescriptorEnumerator = class
    fList: TCEPlugDescriptorList;
    fIndex: Integer;
    function getCurrent: TPlugDescriptor;
    Function moveNext: boolean;
    property current: TPlugDescriptor read getCurrent;
  end;

  operator enumerator(aPlugDescrList: TCEPlugDescriptorList): TPlugDescriptorEnumerator;

implementation

function TCEPlugDescriptorList.getPlugin(index: integer): TPlugDescriptor;
begin
  result := TPlugDescriptor(Items[index]^);
end;

procedure TCEPlugDescriptorList.addPlugin(aValue: PPlugDescriptor);
begin
  add(Pointer(aValue));
end;

function TPlugDescriptorEnumerator.getCurrent: TPlugDescriptor;
begin
  result := fList.plugin[fIndex];
end;

function TPlugDescriptorEnumerator.moveNext: boolean;
begin
  Inc(fIndex);
  result := fIndex < fList.Count;
end;

operator enumerator(aPlugDescrList: TCEPlugDescriptorList): TPlugDescriptorEnumerator;
begin
  result := TPlugDescriptorEnumerator.Create;
  result.fList := aPlugDescrList;
  result.fIndex := -1;
end;

end.
