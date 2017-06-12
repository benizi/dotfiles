<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <!-- example stylesheet for removing one kind of tag -->
  <!-- recursively process every node and attribute -->
  <xsl:template match="node()|@*">
    <!-- copy it, but apply templates to sub-{nodes,attrs} -->
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
  <!-- transform a particular tag (in this case, transform it to nothing) -->
  <xsl:template match="key"/>
</xsl:stylesheet>
