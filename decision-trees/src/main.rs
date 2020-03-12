use image::{imageops::FilterType, DynamicImage, GenericImageView, GrayImage};
use plotters::prelude::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let output = "target/output.png";
    let root = BitMapBackend::new(output, (300, 300)).into_drawing_area();

    root.fill(&WHITE)?;

    let img = image::open("assets/fingers.png")?;

    let mut chart = ChartBuilder::on(&root)
        .caption("Fingers!", ("monospace", 30))
        .margin(5)
        .set_label_area_size(LabelAreaPosition::Left, 40)
        .set_label_area_size(LabelAreaPosition::Bottom, 40)
        .build_ranged(0.0..1.0, 0.0..1.0)?;

    chart.configure_mesh().disable_mesh().draw()?;

    let (w, h) = chart.plotting_area().dim_in_pixel();
    let image = DynamicImage::ImageLuma8(pixelize(img)).resize_exact(
        w - w / 10,
        h - h / 10,
        FilterType::Nearest,
    );

    // Doesn't compile?
    // let elem: BitMapElement<_> = ((0.05, 0.95), image).into();
    let elem = BitMapElement::with_owned_buffer(
        (0.05, 0.95),
        image.dimensions(),
        image.to_rgb().into_raw(),
    )
    .unwrap();

    chart.draw_series(std::iter::once(elem))?;
    print!("{}", output);
    Ok(())
}

fn pixelize(img: DynamicImage) -> GrayImage {
    let mut img = img.into_luma();
    for pixel in img.pixels_mut() {
        pixel[0] = if pixel[0] < 52 { 0 } else { 127 };
    }

    img
}
