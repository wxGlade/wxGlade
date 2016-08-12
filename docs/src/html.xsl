<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                version="1.0">

    <!-- Most of this parameters are specific to dblatex -->

    <xsl:param name="generate.toc" select="'book toc,title,figure,table,example'"/>

    <!-- Print the document collaborators (authors, etc.) in a table -->
    <xsl:param name="doc.collab.show" select="0"/>

    <!-- List of titles to display after TOC -->
    <xsl:param name="doc.lot.show"></xsl:param>

    <!-- Depth of the section numbering -->
    <xsl:param name="doc.section.depth">2</xsl:param>

    <!-- Print the revhistory table -->
    <xsl:param name="latex.output.revhistory" select="0"/>

    <!-- How preface section and subsections appear in TOC -->
    <xsl:param name="preface.tocdepth" select="1"/>

    <!-- How deep should recursive sections appear in the TOC? -->
    <xsl:param name="toc.section.depth">2</xsl:param>

    <!-- Use number and title in cross references -->
    <xsl:param name="xref.with.number.and.title" select="1"/>

    <!-- Use number and title in cross references -->
    <xsl:param name="use.id.as.filename" select="1"/>

    <!-- Create chunk even for first toplevel section -->
    <xsl:param name="chunk.first.sections" select="0"></xsl:param>
</xsl:stylesheet>
