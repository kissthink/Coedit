module barclass;

import std.stdio;

class Bar{
    this(){
        version(revision_1) writeln("bar says: revision 1");
        version(revision_2) writeln("bar says: revision 2");
        version(unittest) writeln("bar says: unittest");
        debug writeln("bar says: debug");
        debug(0) writeln("bar says: debug level < 1");
        debug(1) writeln("bar says: debug level < 2");
        debug(2) writeln("bar says: debug level < 3");
        debug(3) writeln("bar says: debug level < 4");
        debug(a) writeln("bar says: debug ident a");
        debug(b) writeln("bar says: debug ident b");
        debug(c) writeln("bar says: debug ident c");
    }
}
