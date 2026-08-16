(function() {
  "use strict";

  function showStatus(status, message) {
    if (!status) return;
    status.textContent = " " + (message || "Copied.");
    window.setTimeout(function() { status.textContent = ""; }, 1800);
  }

  function copyFallback(text) {
    var area = document.createElement("textarea");
    area.value = text;
    area.style.position = "fixed";
    area.style.opacity = "0";
    document.body.appendChild(area);
    area.select();
    document.execCommand("copy");
    area.remove();
  }

  window.cofadCopyText = function(text, status, message) {
    var copied = function() { showStatus(status, message); };
    if (navigator.clipboard && window.isSecureContext) {
      navigator.clipboard.writeText(text).then(copied);
    } else {
      copyFallback(text);
      copied();
    }
  };

  window.cofadCopyCitation = function(format) {
    var source = document.getElementById("cofad-citation-" + format);
    if (!source) return;
    window.cofadCopyText(
      source.value,
      document.getElementById("cofad-copy-status")
    );
  };

  window.cofadCopyReport = function() {
    var report = document.getElementById("cofad-report-text");
    var plainSource = document.getElementById("cofad-report-copy-text");
    if (!report) return;
    var plain = plainSource ? plainSource.value.trim() : report.innerText.trim();
    var html = '<div style="font-family:Arial,sans-serif;white-space:pre-wrap">' +
      report.innerHTML + "</div>";
    var status = document.getElementById("cofad-report-copy-status");

    if (navigator.clipboard && window.ClipboardItem && window.isSecureContext) {
      var item = new window.ClipboardItem({
        "text/html": new Blob([html], {type: "text/html"}),
        "text/plain": new Blob([plain], {type: "text/plain"})
      });
      navigator.clipboard.write([item]).then(function() {
        showStatus(status, "HTML copied.");
      });
      return;
    }

    var holder = document.createElement("div");
    holder.style.position = "fixed";
    holder.style.left = "-10000px";
    holder.innerHTML = html;
    document.body.appendChild(holder);
    var selection = window.getSelection();
    var range = document.createRange();
    range.selectNodeContents(holder);
    selection.removeAllRanges();
    selection.addRange(range);
    document.execCommand("copy");
    selection.removeAllRanges();
    holder.remove();
    showStatus(status, "HTML copied.");
  };

  window.cofadCopyRCode = function() {
    var source = document.getElementById("cofad-r-code-copy-text");
    if (!source) return;
    window.cofadCopyText(
      source.value.trim(),
      document.getElementById("cofad-r-code-copy-status"),
      "R code copied."
    );
  };

  function tableValues(id) {
    var table = document.getElementById(id);
    if (!table) return null;
    return {
      table: table,
      rows: Array.from(table.rows).map(function(row) {
        return Array.from(row.cells).map(function(cell) {
          return cell.innerText.trim();
        });
      })
    };
  }

  function textLength(value) {
    return Array.from(value).length;
  }

  function pad(value, width, alignRight) {
    var padding = " ".repeat(Math.max(0, width - textLength(value)));
    return alignRight ? padding + value : value + padding;
  }

  function fixedWidthTable(id) {
    var extracted = tableValues(id);
    if (!extracted || !extracted.rows.length) return "";
    var rows = extracted.rows;
    var widths = rows[0].map(function(_, column) {
      return Math.max.apply(null, rows.map(function(row) {
        return textLength(row[column] || "");
      }));
    });
    var numeric = Array.from(extracted.table.rows[0].cells).map(function(cell) {
      return cell.classList.contains("cofad-number");
    });
    var formatRow = function(row) {
      return row.map(function(value, column) {
        return pad(value || "", widths[column], numeric[column]);
      }).join("  ").replace(/\s+$/, "");
    };
    var divider = widths.map(function(width) {
      return "-".repeat(width);
    }).join("  ");
    return [formatRow(rows[0]), divider]
      .concat(rows.slice(1).map(formatRow)).join("\n");
  }

  // Backward-compatible tab-separated copy used by the within-design table.
  window.cofadCopyTable = function(id) {
    var extracted = tableValues(id);
    if (!extracted) return;
    var text = extracted.rows.map(function(row) {
      return row.join("\t");
    }).join("\n");
    window.cofadCopyText(
      text,
      document.getElementById(id + "-copy-status")
    );
  };

  window.cofadCopyTablePlain = function(id) {
    window.cofadCopyText(
      fixedWidthTable(id),
      document.getElementById(id + "-copy-status"),
      "Plain text copied."
    );
  };

  function exportTableHtml(table) {
    var clone = table.cloneNode(true);
    clone.removeAttribute("id");
    clone.setAttribute(
      "style",
      "border-collapse:collapse;font-family:Arial,sans-serif;font-size:10pt"
    );
    Array.from(clone.querySelectorAll("th,td")).forEach(function(cell) {
      cell.removeAttribute("title");
      cell.removeAttribute("tabindex");
      cell.style.padding = "4px 7px";
      cell.style.borderBottom = "1px solid #999";
      if (cell.classList.contains("cofad-number")) {
        cell.style.textAlign = "right";
      }
    });
    Array.from(clone.querySelectorAll("th")).forEach(function(cell) {
      cell.style.fontWeight = "bold";
      cell.style.borderTop = "2px solid #333";
      cell.style.borderBottom = "1px solid #333";
    });
    var finalRow = clone.rows[clone.rows.length - 1];
    if (finalRow) {
      Array.from(finalRow.cells).forEach(function(cell) {
        cell.style.borderBottom = "2px solid #333";
      });
    }
    return clone.outerHTML;
  }

  window.cofadCopyTableHtml = function(id) {
    var extracted = tableValues(id);
    if (!extracted) return;
    var html = exportTableHtml(extracted.table);
    var plain = fixedWidthTable(id);
    var status = document.getElementById(id + "-copy-status");
    if (navigator.clipboard && window.ClipboardItem && window.isSecureContext) {
      var item = new window.ClipboardItem({
        "text/html": new Blob([html], {type: "text/html"}),
        "text/plain": new Blob([plain], {type: "text/plain"})
      });
      navigator.clipboard.write([item]).then(function() {
        showStatus(status, "HTML copied.");
      });
      return;
    }

    var holder = document.createElement("div");
    holder.style.position = "fixed";
    holder.style.left = "-10000px";
    holder.innerHTML = html;
    document.body.appendChild(holder);
    var selection = window.getSelection();
    var range = document.createRange();
    range.selectNodeContents(holder);
    selection.removeAllRanges();
    selection.addRange(range);
    document.execCommand("copy");
    selection.removeAllRanges();
    holder.remove();
    showStatus(status, "HTML copied.");
  };

  var crcTable = null;
  function crc32(bytes) {
    if (!crcTable) {
      crcTable = Array.from({length: 256}, function(_, index) {
        var value = index;
        for (var bit = 0; bit < 8; bit += 1) {
          value = (value & 1) ? (0xEDB88320 ^ (value >>> 1)) : (value >>> 1);
        }
        return value >>> 0;
      });
    }
    var crc = 0xFFFFFFFF;
    bytes.forEach(function(byte) {
      crc = crcTable[(crc ^ byte) & 0xFF] ^ (crc >>> 8);
    });
    return (crc ^ 0xFFFFFFFF) >>> 0;
  }

  function zipHeader(size) {
    return new Uint8Array(size);
  }

  function storedZip(files) {
    var encoder = new TextEncoder();
    var localParts = [];
    var centralParts = [];
    var offset = 0;

    files.forEach(function(file) {
      var name = encoder.encode(file.name);
      var data = encoder.encode(file.content);
      var checksum = crc32(data);
      var local = zipHeader(30);
      var localView = new DataView(local.buffer);
      localView.setUint32(0, 0x04034B50, true);
      localView.setUint16(4, 20, true);
      localView.setUint16(6, 0x0800, true);
      localView.setUint16(8, 0, true);
      localView.setUint32(14, checksum, true);
      localView.setUint32(18, data.length, true);
      localView.setUint32(22, data.length, true);
      localView.setUint16(26, name.length, true);
      localParts.push(local, name, data);

      var central = zipHeader(46);
      var centralView = new DataView(central.buffer);
      centralView.setUint32(0, 0x02014B50, true);
      centralView.setUint16(4, 20, true);
      centralView.setUint16(6, 20, true);
      centralView.setUint16(8, 0x0800, true);
      centralView.setUint16(10, 0, true);
      centralView.setUint32(16, checksum, true);
      centralView.setUint32(20, data.length, true);
      centralView.setUint32(24, data.length, true);
      centralView.setUint16(28, name.length, true);
      centralView.setUint32(42, offset, true);
      centralParts.push(central, name);
      offset += local.length + name.length + data.length;
    });

    var centralOffset = offset;
    var centralSize = centralParts.reduce(function(total, part) {
      return total + part.length;
    }, 0);
    var end = zipHeader(22);
    var endView = new DataView(end.buffer);
    endView.setUint32(0, 0x06054B50, true);
    endView.setUint16(8, files.length, true);
    endView.setUint16(10, files.length, true);
    endView.setUint32(12, centralSize, true);
    endView.setUint32(16, centralOffset, true);
    return new Blob(localParts.concat(centralParts, [end]), {
      type: "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    });
  }

  function xmlEscape(value) {
    return value.replace(/&/g, "&amp;").replace(/</g, "&lt;")
      .replace(/>/g, "&gt;").replace(/\"/g, "&quot;")
      .replace(/'/g, "&apos;");
  }

  function wordTableXml(table) {
    var rows = Array.from(table.rows);
    var numeric = Array.from(rows[0].cells).map(function(cell) {
      return cell.classList.contains("cofad-number");
    });
    return rows.map(function(row, rowIndex) {
      var cells = Array.from(row.cells).map(function(cell, column) {
        var bold = rowIndex === 0 ? "<w:b/>" : "";
        var italic = rowIndex === 0 && ["F", "p"].includes(
          cell.innerText.trim()
        ) ? "<w:i/>" : "";
        var shading = rowIndex === 0 ? '<w:shd w:fill="E7E6E6"/>' : "";
        var alignment = numeric[column] ? '<w:jc w:val="right"/>' : "";
        var width = column === 0 ? 3600 : 1500;
        return '<w:tc><w:tcPr><w:tcW w:w="' + width +
          '" w:type="dxa"/>' + shading + "</w:tcPr><w:p><w:pPr>" +
          alignment + "</w:pPr><w:r><w:rPr>" + bold + italic +
          '<w:sz w:val="18"/></w:rPr><w:t xml:space="preserve">' +
          xmlEscape(cell.innerText.trim()) + "</w:t></w:r></w:p></w:tc>";
      }).join("");
      return "<w:tr>" + cells + "</w:tr>";
    }).join("");
  }

  window.cofadDownloadTableDocx = function(id, filename) {
    var extracted = tableValues(id);
    if (!extracted) return;
    var documentXml = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
      '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">' +
      "<w:body><w:p><w:r><w:rPr><w:b/></w:rPr>" +
      "<w:t>Variance decomposition (F table)</w:t></w:r></w:p>" +
      '<w:tbl><w:tblPr><w:tblBorders><w:top w:val="single" w:sz="8"/>' +
      '<w:left w:val="nil"/><w:bottom w:val="single" w:sz="8"/>' +
      '<w:right w:val="nil"/><w:insideH w:val="single" w:sz="2"/>' +
      '<w:insideV w:val="nil"/></w:tblBorders></w:tblPr>' +
      '<w:tblGrid><w:gridCol w:w="3600"/>' +
      '<w:gridCol w:w="1500"/><w:gridCol w:w="1500"/>' +
      '<w:gridCol w:w="1500"/><w:gridCol w:w="1500"/>' +
      '<w:gridCol w:w="1500"/><w:gridCol w:w="1500"/>' +
      '<w:gridCol w:w="1500"/></w:tblGrid>' +
      wordTableXml(extracted.table) + "</w:tbl>" +
      '<w:sectPr><w:pgSz w:w="15840" w:h="12240" w:orient="landscape"/>' +
      '<w:pgMar w:top="720" w:right="720" w:bottom="720" w:left="720"/>' +
      "</w:sectPr></w:body></w:document>";
    var blob = storedZip([
      {
        name: "[Content_Types].xml",
        content: '<?xml version="1.0" encoding="UTF-8"?>' +
          '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">' +
          '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>' +
          '<Default Extension="xml" ContentType="application/xml"/>' +
          '<Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>' +
          "</Types>"
      },
      {
        name: "_rels/.rels",
        content: '<?xml version="1.0" encoding="UTF-8"?>' +
          '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">' +
          '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/>' +
          "</Relationships>"
      },
      {name: "word/document.xml", content: documentXml}
    ]);
    var link = document.createElement("a");
    link.href = URL.createObjectURL(blob);
    link.download = filename || "cofad-f-table.docx";
    document.body.appendChild(link);
    link.click();
    link.remove();
    window.setTimeout(function() { URL.revokeObjectURL(link.href); }, 1000);
    showStatus(
      document.getElementById(id + "-copy-status"),
      "DOCX downloaded."
    );
  };

  var hotTableIds = [
    "hot_model", "hot_lambda_between", "hot_lambda_within"
  ];

  function renderHotTables() {
    hotTableIds.forEach(function(id) {
      var element = document.getElementById(id);
      var widget = element && window.HTMLWidgets ?
        window.HTMLWidgets.getInstance(element) : null;
      if (widget && widget.hot && element.offsetParent !== null) {
        widget.hot.render();
      }
    });
  }

  function scheduleHotTableRender() {
    [0, 100, 350].forEach(function(delay) {
      window.setTimeout(renderHotTables, delay);
    });
  }

  window.cofadRenderHotTables = scheduleHotTableRender;
  if (window.jQuery) {
    window.jQuery(document).on("shiny:value", function(event) {
      if (hotTableIds.includes(event.name)) scheduleHotTableRender();
    });
    window.jQuery(document).on(
      "change",
      "#compare_competing",
      scheduleHotTableRender
    );
    window.jQuery(document).on("shown.bs.collapse", scheduleHotTableRender);
  }
  window.addEventListener("resize", scheduleHotTableRender);
})();
