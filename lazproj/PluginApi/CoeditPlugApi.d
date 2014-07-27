module CoeditPlugApi;

    alias void* Host_t;
    alias void* Plugin_t;

    // API version
    immutable uint CE_PLG_API_VER = 0;

    /// When passed to the Coedit dispatcher, it shows a 'hello plugin' message.
    immutable uint HELLO_PLUGIN    = 0xFFFFFFFF;

// Denotes the emiter and the message kind -------------------------------------

    /// Coedit sends an event.
    immutable uint HOST_EVENT      = 0x10000000;
    /// Coedit sends some data.
    immutable uint HOST_DATA       = 0x20000000;
    /// The plug-in sends an event.
    immutable uint PLUG_EVENT      = 0x30000000;
    /// The plug-in sends some data.
    immutable uint PLUG_DATA       = 0x40000000;

// Denotes the message context -------------------------------------------------

    /// the dispatcher call is related to the project(s)
    immutable uint CTXT_PROJ  = 0x01000000;
    /// the dispatcher call is related to the document(s)
    immutable uint CTXT_DOCS  = 0x02000000;
    /// the dispatcher call is related to the edition of a document.
    immutable uint CTXT_EDIT  = 0x03000000;
    /// the dispatcher call is related to the Coedit 'Message Widget'.
    immutable uint CTXT_MSGS  = 0x04000000;
    /// the dispatcher call is related to the Coedit dialogs.
    immutable uint CTXT_DLGS  = 0x05000000;

// The events kinds ------------------------------------------------------------

    /// somethings's just changed.
    immutable uint EV_CHANGED   = 0x00000001;
    /// something's just been selected.
    immutable uint EV_FOCUSED   = 0x00000002;
    /// something's just been closed.
    immutable uint EV_CLOSED    = 0x00000003;
    /// something's just been created.
    immutable uint EV_NEW       = 0x00000004;
    /// something gonna be compiled.
    immutable uint EV_COMPILE   = 0x00000005;
    /// something gonna be executed.
    immutable uint EV_RUN       = 0x00000006;

// The data kinds --------------------------------------------------------------

    /// data1 is used to set/get a filename. data1 is a PChar. data0 represents an index.
    immutable uint DT_FNAME     = 0x00000001;
    /// data0 represents a count.
    immutable uint DT_COUNT     = 0x00000002;

    /// data1 is used to set a message. data1 is a PChar.
    immutable uint DT_ERR       = 0x00000001;
    /// ditto
    immutable uint DT_INF       = 0x00000002;
    /// ditto
    immutable uint DT_WARN      = 0x00000003;

// terminal opCodes (emiter + context + event/data kind) -----------------------

    /// Coedit says that the project's just been modified.
    immutable uint HOST_PROJ_CHANGED = HOST_EVENT + CTXT_PROJ + EV_CHANGED;

    /// opCode for asking for a document filename. data0 must be the document index.
    immutable uint PLUG_WANT_DOC_NAME  = PLUG_EVENT + CTXT_DOCS + DT_FNAME;

    /// opCode for getting a document filenmae. data1 is a PChar to the filename.
    immutable uint HOST_GIVE_DOC_NAME  = HOST_DATA + CTXT_DOCS + DT_FNAME;

    /// opCodes for displaying a message in a dialog box.
    immutable uint PLUG_DLGS_ERR    = PLUG_DATA + CTXT_DLGS + DT_ERR;
    /// ditto.
    immutable uint PLUG_DLGS_WARN   = PLUG_DATA + CTXT_DLGS + DT_WARN;
    /// ditto.
    immutable uint PLUG_DLGS_INF    = PLUG_DATA + CTXT_DLGS + DT_INF;

    /// opCodes for displaying a message in the 'Message Widget'.
    immutable uint PLUG_MSGS_ERR    = PLUG_DATA + CTXT_MSGS + DT_ERR;
    /// ditto.
    immutable uint PLUG_MSGS_WARN   = PLUG_DATA + CTXT_MSGS + DT_WARN;
    /// ditto.
    immutable uint PLUG_MSGS_INF    = PLUG_DATA + CTXT_MSGS + DT_INF;

// host-side prototypes --------------------------------------------------------

    /**
     * A plugin asks for some information or it passes data here.
     * Data1 and data2 are some pointers to a particular variable type.
     * In the plugin hostCreatePlugProc, a the location of COedit dispatcher is passed
     * to the plugin.
     */
    extern(C) alias plugDispatchToHostProc =
        void function(Plugin_t aPlugin, uint opCode, uint data0, void* data1, void* data2);

// plugin-side prototypes-------------------------------------------------------

    /**
     * Coedit initializes a plugin, the result is passed during the runtime as "aTarget".
     * In the plugin implementation, it must be named 'createPlug'.
     * If the result is null then the Plugin is not used at all.
     * If the plugin is not warped in a class than the result must be set on something
     * that can be pointed to (e.g: a global variable).
     */
    extern(C) alias hostCreatePlugProc =
        Plugin_t function (plugDispatchToHostProc aHost);

    /**
     * Coedit closes and aTarget can be destroyed.
     * In the plugin implementation, it must be named 'destroyPlug'.
     */
    extern(C) alias hostDestroyPlugProc =
        void function (Plugin_t aTarget);

    /**
     * Coedit events and data are passed here. data1 and data2 can be casted according to opCode.
     * In the plugin implementation, it must be named 'dispatchToPlug'.
     */
    extern(C) alias hostDispatchToPlugProc =
        void function(Plugin_t aPlugin, uint opCode, uint data0, void* data1, void* data2);

// helpers ---------------------------------------------------------------------

uint getEmiter(in uint opCode){
    return opCode & 0xF0000000;
}

uint getContext(in uint opCode){
    return opCode & 0x0FF00000;
}

uint getCommand(in uint opCode){
    return opCode & 0x000FFFFF;
}

