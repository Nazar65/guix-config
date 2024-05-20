<?php declare(strict_types=1);
/**
 * PHP Coding Standards fixer configuration
 */

$finder = PhpCsFixer\Finder::create()
    ->name('*.phtml')
    ->exclude('dev/tests/integration/tmp')
    ->exclude('dev/tests/integration/var')
    ->exclude('lib/internal/Cm')
    ->exclude('lib/internal/Credis')
    ->exclude('lib/internal/Less')
    ->exclude('lib/internal/LinLibertineFont')
    ->exclude('pub/media')
    ->exclude('pub/static')
    ->exclude('setup/vendor')
    ->exclude('var');

return (new PhpCsFixer\Config())
    ->setFinder($finder)
    ->setCacheFile('/home/nazar/.emacs.d/php-cs-fixer/.php_cs.cache')
    ->setRules([
        '@PSR2' => true,
        'array_syntax' => ['syntax' => 'short'],
        'concat_space' => ['spacing' => 'one'],
        'method_argument_space' => [
            'attribute_placement' => 'standalone',
            'on_multiline' => 'ensure_fully_multiline'
        ],
        'include' => true,
        'no_spaces_around_offset' => true,
        'new_with_braces' => true,
        'visibility_required' => true,
        'align_multiline_comment' => true,
        'no_extra_blank_lines' => true,
        'no_empty_statement' => true,
        'phpdoc_add_missing_param_annotation' => true,
        'no_leading_import_slash' => true,
        'class_definition' => true,
        'no_leading_namespace_whitespace' => true,
        'no_multiline_whitespace_around_double_arrow' => true,
        'no_blank_lines_after_class_opening' => true,
        'multiline_whitespace_before_semicolons' => true,
        'no_singleline_whitespace_before_semicolons' => true,
        'no_trailing_comma_in_singleline_array' => true,
        'no_unused_imports' => true,
        'no_whitespace_in_blank_line' => true,
        'phpdoc_align' => ['align' => 'left'],
        'phpdoc_indent' => true,
        'object_operator_without_whitespace' => true,
        'ordered_imports' => true,
        'binary_operator_spaces' => true,
        'no_spaces_inside_parenthesis' => true,
        'fully_qualified_strict_types' => true,
        'single_line_after_imports' => true,
        'no_whitespace_before_comma_in_array' => true,
        'standardize_not_equals' => true,
        'array_indentation' => true,
        'ternary_operator_spaces' => true,
        'class_attributes_separation' => true
    ]);
