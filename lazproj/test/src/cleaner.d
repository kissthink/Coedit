module cleaner;

import std.stdio;
import std.path;
import std.file;
import std.getopt;
import std.array;

//as a runnable: -f "dir" -r -e ".ext1,.ext2"

/*
as a post-build process, parameter list:
-f
pathtoclean (w/o dbl quotes)
-r
-e
.a,.b,.c
*/
void main(string args[])
{
    bool recursive;
    string root;
    string userexts;
    string exts[];

    exts ~= [".obj", ".o"];

    getopt(args, config.passThrough,
        "f|from", &root,
        "r|recursive", &recursive,
        "e|ext", &userexts
    );

    if (!exists(root) || root.length == 0)
        return;

    if (userexts.length != 0)
    {
        auto itms = split(userexts, ',');
        foreach(itm; itms)
            if (itm.length > 1 && itm[0] == '.')
                exts ~= itm;
    }

    void clean(in string currroot)
    {
        DirIterator entries = dirEntries(currroot, SpanMode.shallow);

        foreach(entry; entries)
        {
            scope(failure){}

            if (!isDir(entry))
            {
                foreach(trg_ext; exts)
                    if (entry.extension == trg_ext)
                        std.file.remove(entry);
            }
            else if (recursive)
                clean(entry);
        }
    }

    clean(root);
}
