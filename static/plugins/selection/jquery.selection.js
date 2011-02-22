/**
 * jQuery.selection - jQuery Plugin
 *
 * Under The MIT License
 * Copyright (c) 2010 Iwasaki. (http://d.hatena.ne.jp/ja9/)
 * 
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Version: 1.0 beta
 * Revision: $Rev$
 * Date: $Date$
 */
(function($) {
  var selectionFunctions = {
        getCaretData: function(element) {
          var res = {
                text: '',
                start: 0,
                end: 0
              };

          if (!element.value) {
            // 値がない、もしくは空文字列
            return res;
          }

          try {
            if (window.getSelection) {
              // IE 以外
              res.start = element.selectionStart;
              res.end = element.selectionEnd;
              res.text = element.value.slice(res.start, res.end);
            } else if (document.selection) {
              // IE
              element.focus();

              var range = document.selection.createRange(),
                  range2 = document.body.createTextRange(),
                  tmpLength;

              res.text = range.text;

              range2.moveToElementText(element);
              // EndToStart だと後ろの改行がトリムされる
              range2.setEndPoint('StartToStart', range);

              res.start = element.value.length - range2.text.length;
              res.end = res.start + range.text.length;
            }
          } catch (e) {
          }

          return res;
        },

        getCaret: function(element) {
          var tmp = this.getCaretData(element);
          return {start: tmp.start, end: tmp.end};
        },

        getText: function(element) {
          return this.getCaretData(element).text;
        },

        caretMode: function(caret) {
          caret = caret || 'keep';
          if (caret == false) {
            caret = 'end';
          }

          switch (caret) {
            case 'keep':
            case 'start':
            case 'end':
              break;

            default:
              caret = 'keep';
          }

          return caret;
        },

        replace: function(element, text, caret) {
          var tmp = this.getCaretData(element),
              orig = element.value,
              pos = $(element).scrollTop(),
              range = {start: tmp.start, end: tmp.start + text.length};

          element.value = orig.substr(0, tmp.start) + text + orig.substr(tmp.end);

          $(element).scrollTop(pos);
          this.setCaret(element, range, caret);
          },

        insertBefore: function(element, text, caret) {
          var tmp = this.getCaretData(element),
              orig = element.value,
              pos = $(element).scrollTop(),
              range = {start: tmp.start + text.length, end: tmp.end + text.length};

          element.value = orig.substr(0, tmp.start) + text + orig.substr(tmp.start);

          $(element).scrollTop(pos);
          this.setCaret(element, range, caret);
        },

        insertAfter: function(element, text, caret) {
          var tmp = this.getCaretData(element),
              orig = element.value,
              pos = $(element).scrollTop(),
              range = {start: tmp.start, end: tmp.end};

          element.value = orig.substr(0, tmp.end) + text + orig.substr(tmp.end);

          $(element).scrollTop(pos);
          this.setCaret(element, range, caret);
        },

        setCaret: function(element, toRange, caret) {
          caret = this.caretMode(caret);

          if (caret == 'start') {
            toRange.end = toRange.start;
          } else if (caret == 'end') {
            toRange.start = toRange.end;
          }

          element.focus();
          try {
            if (element.createTextRange) {
              var range = element.createTextRange();

              if (window.navigator.userAgent.toLowerCase().indexOf("msie") >= 0) {
                toRange.start = element.value.substr(0, toRange.start).replace(/\r/g, '').length;
                toRange.end = element.value.substr(0, toRange.end).replace(/\r/g, '').length;
              }

              range.collapse(true);
              range.moveStart('character', toRange.start);
              range.moveEnd('character', toRange.end - toRange.start);

              range.select();
            } else if (element.setSelectionRange) {
              element.setSelectionRange(toRange.start, toRange.end);
            }
          } catch (e) {
          }
        }
      },

      selectionMethods = {
        getSelection: function(mode) {
          var getText = ((mode || 'text').toLowerCase() == 'text');

          try {
            if (window.getSelection) {
              if (getText) {
                // get text
                return window.getSelection().toString();
              } else {
                // get html
                var sel = window.getSelection(), range;

                if (sel.getRangeAt) {
                  range = sel.getRangeAt(0);
                } else {
                  range = document.createRange();
                  range.setStart(sel.anchorNode, sel.anchorOffset);
                  range.setEnd(sel.focusNode, sel.focusOffset);
                }

                return $('<div></div>').append(range.cloneContents()).html();
              }
            } else if (document.selection) {
              if (getText) {
                // get text
                return document.selection.createRange().text;
              } else {
                // get html
                return document.selection.createRange().htmlText;
              }
            }
          } catch (e) {
          }

          return '';
        }
      },

      selectionPrototypeMethods = {
        getSelection: function() {
          var res = [];

          this.each(function() {
            res.push(selectionFunctions.getText(this));
          });

          return (res.length < 2) ? res[0] : res;
        },

        replaceSelection: function(text, caret) {
          return this.each(function() {
            selectionFunctions.replace(this, text, caret);
          });
        },

        insertBeforeSelection: function(text, caret) {
          return this.each(function() {
            selectionFunctions.insertBefore(this, text, caret);
          });
        },

        insertAfterSelection: function(text, caret) {
          return this.each(function() {
            selectionFunctions.insertAfter(this, text, caret);
          });
        },

        getCaretPos: function() {
          var res = [];

          this.each(function() {
            res.push(selectionFunctions.getCaret(this));
          });

          return (res.length < 2) ? res[0] : res;
        },

        setCaretPos: function(range) {
          return this.each(function() {
            selectionFunctions.setCaret(this, range);
          });
        }
      };

  $.extend(selectionMethods);
  $.fn.extend(selectionPrototypeMethods);
})(jQuery);
