(function ($) {

    $(function () {
        if (!supportsInputAttribute('placeholder')) {
            $('[placeholder]').each(function () {
                var $this = $(this),
                    $form = $this.closest('form'),
                    placeholderText = $this.attr('placeholder'),
                    placeholderColor = 'silver',
                    defaultColor = $this.css('color');
                // datepickerと連携するため.
                $this.attr('defaultColor', defaultColor);
                $this.bind({
                    focus: function () {
                        if ($this.val() === placeholderText) {
                            $this.val('').css('color', defaultColor);
                        }
                    },
                    blur: function () {
                        if ($this.val() === '') {
                            $this.val(placeholderText).css('color', placeholderColor);
                        } else if ($this.val() === placeholderText) {
                            $this.css('color', placeholderColor);
                        }
                    }
                });
                $this.trigger('blur');
                $form.submit(function () {
                    if ($this.val() === placeholderText) {
                        $this.val('');
                    }
                });
            });
        }
    });

    // detect support for input attirbute
    function supportsInputAttribute (attr) {
        var input = document.createElement('input');
        return attr in input;
    }

})(jQuery);
