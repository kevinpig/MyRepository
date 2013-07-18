#! /usr/bin/env python
 # -*- coding: utf8 -*-

class Tag:
    n_tag = None

    tag = None

    name = None

    un_escaped_name = None

    vr = None

    vm = None

    retired = None

    var_name = None

    dicom_var_name = None


class SopClass(object):
    name = None

    uid = None

    type = None

    var_name = None


def format_to_pascal(val):
    """
    @type val: string
    """
    if val is None:
        return None

    out_string = ""
    for c in val:
        if c == " ":
            last_char_was_space = True
        elif last_char_was_space:
            out_string += c.upper()
        else:
            out_string += c.lower()

    return out_string


def create_variable_name(input):
    """
    @type input: string
    """
    pass


def create_name(tag):
    pass


class Parser:
    sop_classes = []

    meta_sop_classes = []

    transfer_syntaxes = []

    def parse_file(self, filename):
        from xml.etree import cElementTree

        start_of_table = False
        column_count = -1
        column_array = [None for i in range(1, 11)]
        is_tag = False
        is_uid = False

        for event, elem in cElementTree.iterparse(filename, ('start', 'end')):
            if event == 'start' and elem.tag == '{http://schemas.microsoft.com/office/word/2003/wordml}tbl':
                start_of_table = True
                print "start_of_table"
                is_first = True

            if event == 'end' and elem.tag == "{http://schemas.microsoft.com/office/word/2003/wordml}tbl":
                print "stop_of_table"
                start_of_table = False

            if start_of_table:
                if event == 'start' and elem.tag == "{http://schemas.microsoft.com/office/word/2003/wordml}tc":
                    column_count += 1
                elif event == 'start' and elem.tag == "{http://schemas.microsoft.com/office/word/2003/wordml}t":
                    val = ""
                    if elem.text is not None:
                        val = elem.text

                    if column_array[column_count] is None:
                        column_array[column_count] = val
                    else:
                        column_array[column_count] += val

                if event == 'end' and elem.tag == "{http://schemas.microsoft.com/office/word/2003/wordml}tr":
                    if is_first:
                        if column_array[0] is None:
                            pass
                        elif column_array[0] == "Tag":
                            is_tag = True
                            is_uid = False
                            is_first = False
                        elif column_array[0].startswith("UID"):
                            is_tag = False
                            is_uid = True
                            is_first = False
                    else:
                        if is_tag:
                            if column_array[0] is not None and column_array[0] != "Tag" and column_array[0] != "":
                                tag = Tag()
                                tag.tag = column_array[0]
                                tag.name = column_array[1]
                                tag.dicom_var_name = column_array[2]
                                if column_array[3] is not None:
                                    tag.vr = column_array[3].strip()
                                if column_array[4] is not None:
                                    tag.vm = column_array[4].strip()
                                tag.retired = column_array[5]

                                import re
                                nodes = re.split('[(,)]', tag.tag)
                                nodes = [n for n in nodes if n != ""]
                                group, element = int(nodes[0], 16), int(nodes[1], 16)

                                tag.n_tag = element | group << 16

                        elif is_uid:
                            uid = SopClass()
                            uid.uid = column_array[0]
                            uid.name = column_array[1]
                            uid.type = column_array[2]

                            uid.var_name = create_variable_name(uid.name)

                            if uid.type == "SOP Class":
                                uid.var_name = "Sop" + uid.var_name

                                self.sop_classes.append((uid.name, uid))
                            elif uid.type == "Transfer Syntax":
                                index = uid.var_name.index(":")

                                self.transfer_syntaxes.append((uid.name, uid))
                            elif uid.type == "Meta SOP class":
                                self.meta_sop_classes.append((uid.name, uid))

                            print uid.uid, uid.name, uid.type

                    column_count = -1
                    column_array = [None for i in range(1, 11)]


class CodeGenerator:
    sop_list = []

    meta_sop_list = []

    def write_header(self):
        pass

    def write_sop_classes(self, filename):
        """
        @type filename: string
        """
        f = open()

    pass

if __name__ == '__main__':
    import sys

    parse = Parser()

    parse.parse_file(r'08_06pu.xml')



