#ifdef HAVE_OPENCV_TEXT
#include <opencv2/text/ocr.hpp>

// Implementation of OCRTesseract::runWithComponents — declared in the
// patched ocr.hpp header so the binding generator picks it up.
//
// This non-virtual method delegates to the virtual run() overload that
// accepts output vectors, converting InputArray → Mat on the way in and
// std::string → cv::String on the way out.
void cv::text::OCRTesseract::runWithComponents(
    InputArray image,
    String& output_text,
    std::vector<Rect>& component_rects,
    std::vector<String>& component_texts,
    std::vector<float>& component_confidences,
    int component_level)
{
    Mat mat = image.getMat();
    std::string text;
    std::vector<std::string> texts;

    run(mat, text, &component_rects, &texts, &component_confidences, component_level);

    output_text = text;
    component_texts.clear();
    component_texts.reserve(texts.size());
    for (const auto& t : texts) {
        component_texts.push_back(t);
    }
}

#endif
