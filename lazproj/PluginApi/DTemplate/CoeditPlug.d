module CoeditPlug;

/*
Under linux:

Build:
1:dmd -c CoeditPlug.d ../CoeditPlugApi.d pathtoIZ/types.d -fPIC
(or with a coedit project, select outputKind : object (which is actually -c))
2:dmd -ofCOeditPlug.so <the three .o> -shared -defaultlib=libphobos2.so

Note about IZ:

The GC will make the plugin crash if coeditPlug is not an izObject
because (?). However since izObjects have their own, G.C free, de/allocators
it just instanciates fine.
*/


import std.stdio, std.string, std.conv;
import CoeditPlugApi;
import iz.types;

version(Posix)
{
}
else
{
    import std.c.windows.windows;
    import core.sys.windows.dll;
}


coeditPlug plugin;

class coeditPlug: izObject
{
    protected
    {
        plugDispatchToHostProc fDispatcher;
        Plugin_t asPlugin_t(){return cast(Plugin_t)&this;}
    }
    public
    {
        this(plugDispatchToHostProc aDispatcher)
        {
            assert(aDispatcher, "the Coedit dispatcher is missing");
            fDispatcher = aDispatcher;
            //fDispatcher(asPlugin_t, HELLO_PLUGIN, 0, null, null);

            auto msg = "simple Coedit plugin is created".toStringz;
            fDispatcher(asPlugin_t, PLUG_MSGS_INF, 0, cast(void*)msg, null);
        }

        void hostEvent(uint eventContext, uint eventKind)
        {
            auto msg = (to!string(eventContext) ~ " " ~ to!string(eventKind)).toStringz;
            fDispatcher(asPlugin_t, PLUG_MSGS_INF, 0, cast(void*)msg, null);
        }

        void hostData(uint dataContext, uint dataKind, uint data0, void* data1, void* data2)
        {
        }
    }
}


extern(C) export
Plugin_t createPlug(plugDispatchToHostProc aHost)
{
    plugin = new coeditPlug(aHost);
    return &plugin;
}

extern(C) export
void destroyPlug(Plugin_t aPlug)
{
    delete plugin;
}

extern(C) export
void dispatchToPlug(Plugin_t aPlugin, uint opCode, uint data0, void* data1, void* data2)
{
    assert(&plugin == aPlugin);
    coeditPlug* plg = (cast(coeditPlug*) aPlugin);

    auto emit = opCode.getEmiter;
    auto ctxt = opCode.getContext;
    auto comd = opCode.getCommand;

    if (emit == HOST_EVENT)
        plg.hostEvent(ctxt, comd);
    else
        plg.hostData(ctxt, comd, data0, data1, data2);
}

version(Posix)
{
}
else
{
    __gshared HINSTANCE g_hInst;

    extern (Windows)
    BOOL DllMain(HINSTANCE hInstance, ULONG ulReason, LPVOID pvReserved)
    {
        switch (ulReason)
        {
	      case DLL_PROCESS_ATTACH:
	          g_hInst = hInstance;
	          dll_process_attach( hInstance, true );
	          break;

	      case DLL_PROCESS_DETACH:
	          dll_process_detach( hInstance, true );
	          break;

	      case DLL_THREAD_ATTACH:
	          dll_thread_attach( true, true );
	          break;

	      case DLL_THREAD_DETACH:
	          dll_thread_detach( true, true );
	          break;

          default: break;
        }
        return true;
    }
}
