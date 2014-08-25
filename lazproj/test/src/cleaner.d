module cleaner;

import std.stdio;
import std.path;
import std.file;
import std.getopt;
import std.array;

/*

as a runnable, input query line:

    -e ".ext1,.ext2" -f "file.ext,file.ext" -p "rootPath" -r

or as a post-build process, parameter list:

    -e
    ".ext1,.ext2"
    -f
    "file.ext,file.ext"
    -p
    "rootPath"
    -r

*/
void main(string args[])
{
    bool recursive;
    string path;
    string userexts;
    string userfiles;
    string exts[];
    string files[];

    exts ~= [".obj", ".o"];

    getopt(args, config.passThrough,
        "e|ext",    &userexts,
        "f|files",  &userfiles,
        "p|path",   &path,
        "r|recursive", &recursive
    );

    if (!exists(path) || path.length == 0)
        return;

    if (userexts.length != 0)
    {
        auto itms = split(userexts, ',');
        foreach(itm; itms)
            if (itm.length > 1 && itm[0] == '.')
                exts ~= itm;
    }

    if (userfiles.length != 0)
    {
        auto itms = split(userfiles, ',');
        foreach(itm; itms)
            if (itm.length > 0)
                files ~= itm;
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
                foreach(trg_file; files)
                    if (entry.name == trg_file)
                        std.file.remove(entry);

            }
            else if (recursive)
                clean(entry);
        }
    }
    clean(path);
}
