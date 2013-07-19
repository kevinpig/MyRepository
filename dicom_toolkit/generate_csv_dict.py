#!/usr/bin/env python
#-*- coding:utf8 -*-


def parse_file(filename):
    """
    @type filename: string
    """
    from xml.etree import cElementTree

    start_of_table = False
    column_count = -1
    column_array = [None for i in range(1, 11)]

    is_tag = False
    is_uid = False

    tag_file = open("tag_dict_2011.csv", "wb")
    uid_file = open("uid_dict_2011.csv", "wb")

    for event, elem in cElementTree.iterparse(filename, ("start", "end")):
        if event == 'start' and elem.tag == '{http://schemas.openxmlformats.org/wordprocessingml/2006/main}tbl':
            start_of_table = True
            is_first = True
            print "start_of_table"

        if event == 'end' and elem.tag == "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}tbl":
            print "stop_of_table"
            start_of_table = False

        if start_of_table:
            if event == 'start' and elem.tag == "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}tc":
                column_count += 1
            elif event == 'start' and elem.tag == "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}t":
                val = ""
                if elem.text is not None:
                    val = elem.text

                if column_array[column_count] is None:
                    column_array[column_count] = val
                else:
                    column_array[column_count] += val

            if event == 'end' and elem.tag == "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}tr":
                if is_first:
                    if column_array[0] is None:
                        is_tag = False
                        is_uid = False
                    else:
                        if column_array[0] == "Tag":
                            is_tag = True
                            is_uid = False
                        elif column_array[0].startswith("UID"):
                            is_uid = True
                            is_tag = False

                        is_first = False
                else:
                    if is_tag:
                        content = ','.join([item for item in column_array if item is not None]).encode("utf8").strip()
                        if content != "" and not content.startswith("Tag"):
                            print content
                            tag_file.write(content + "\n")
                    elif is_uid:
                        content = ','.join([item for item in column_array if item is not None]).encode("utf8").strip()
                        if content != "" and not content.startswith("UID"):
                            print content
                            uid_file.write(content + "\n")

                column_count = -1
                column_array = [None for i in range(1, 11)]

    tag_file.close()
    uid_file.close()


if __name__ == '__main__':
    import sys
    parse_file(sys.argv[1])