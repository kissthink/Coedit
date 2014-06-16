module fooclass;

import std.stdio;

class Foo{
    this(){
        version(revision_1) writeln("foo says: revision 1");
        version(revision_2) writeln("foo says: revision 2");
        version(unittest) writeln("foo says: unittest");
        debug writeln("foo says: debug");
        debug(0) writeln("foo says: debug level < 1");
        debug(1) writeln("foo says: debug level < 2");
        debug(2) writeln("foo says: debug level < 3");
        debug(3) writeln("foo says: debug level < 4");

    }
}
