from aetherling.helpers.nameCleanup import cleanName
from aetherling.modules.native_linebuffer.any_dimensional_native_linebuffer import AnyDimensionalLineBuffer, DefineAnyDimensionalLineBuffer
from aetherling.modules.delayed_buffer import DelayedBuffer
from mantle.common.countermod import SizedCounterModM
from mantle import DefineCoreirConst
from magma import *
from magma.backend.coreir_ import CoreIRBackend



def DefineTwoDimensionalLineBuffer(
        cirb: CoreIRBackend,
        pixel_type: Kind,
        pixels_per_row_per_clock: int,
        rows_of_pixels_per_clock: int,
        window_cols: int,
        window_rows: int,
        image_cols: int,
        image_rows: int,
        stride_cols: int,
        stride_rows: int,
        origin_cols: int,
        origin_rows: int) -> Circuit:
    """
    :param cirb: The CoreIR backend currently be used
    :param pixel_type: the type of each pixel. A type of Array(3, Array(8, Bit)) is for
    3 color channel, 8 bits per channel.
    :param pixels_per_row_per_clock: The number of pixels per row of the image the linebuffer
    receives as input each clock.
    :param rows_of_pixels_per_clock: The number of rows of pixels of the image the linebuffer
    receives as input each clock.
    :param window_cols: The number of columns in the stencils that are emitted
    :param window_rows: The number of rows in the stencils that are emitted
    :param image_cols: The number of columns in the 2D input image
    :param image_rows: The number of rows in the 2D input image
    :param stride_cols: The distance between origins of consecutive stencils in number of pixels
    in the columns dimension. A stride of 1 means that successive stencils have origins in adjacent
    columns.
    :param stride_rows: The distance between origins of consecutive stencils in number of pixels
    in the rows dimension. A stride of 1 means that successive stencils have origins in adjacent
    rows.
    :param origin_cols: The column of the first pixel of the first window relative to
    the top left corner of the image
    :param origin_rows: The row of the first pixel of the first window relative to
    the top left corner of the image

    Restrictions:

    1. image_cols % pixels_per_row_per_clock == 0
    2. image_rows % rows_of_pixels_per_clock == 0
    3. if rows_of_pixels_per_clock > 1, then image_cols == pixels_per_row_per_clock
    4. image_cols % stride_cols == 0
    5. image_rows % stride_rows == 0
    6. (stride_cols * stride_rows) %
         (pixels_per_row_per_clock * rows_of_pixels_per_clock) == 0 OR
       (pixels_per_row_per_clock * rows_of_pixels_per_clock) %
         (stride_cols * stride_rows) == 0
    7. window_cols > |origin_cols|
    8. window_rows > |origin_rows|
    9. origin_cols <= 0
    10. origin_rows <= 0
    11. window_cols - origin_cols < image_cols
    12. window_rows - origin_rows < image_rows

    :return: A 2D Linebuffer with ports I, O, valid, CE, and next_row (if last_row false)
    """
    class _LB(Circuit):
        if image_cols % pixels_per_row_per_clock != 0:
            reason = """
            this is necessary so that input a complete row before getting 
            the input for the next row. This means don't have a clock cycle
            where input pixels are from two different rows in the input image.
            """
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: image_cols {} not divisible by"
                            "pixels_per_row_per_clock {}. \n Reason {}"
                            .format(image_cols, pixels_per_row_per_clock, reason))

        if image_rows % rows_of_pixels_per_clock != 0:
            reason = """
            this is necessary so that input a complete image with the same number 
            of input pixels every clock, don't have a weird ending
            with only 1 pixel from the image left to input
            """
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: image_rows {} not divisible by"
                            "pixels_per_row_per_clock {}"
                            .format(image_rows, rows_of_pixels_per_clock, reason))

        if image_cols % stride_cols != 0:
            reason = "stride_cols is downsample factor for number of columns." \
                     "This requirement ensures that the column downsample factor" \
                     "cleanly divides the image's number of columns"
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: image_cols {} not divisible by"
                            "stride_cols {}. \n Reason: {}"
                            .format(image_cols, stride_cols, reason))

        if image_rows % stride_rows != 0:
            reason = "stride_rows is downsample factor for number of rows." \
                     "This requirement ensures that the row downsample factor" \
                     "cleanly divides the image's number of rows"
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: image_rows {} not divisible by"
                            "stride_rows {}. \n Reason: {}"
                            .format(image_rows, stride_rows, reason))

        if (stride_cols % pixels_per_row_per_clock != 0 and
                pixels_per_row_per_clock % stride_cols != 0):
            reason = """
            the average number of output windows per row per clock =
                pixels per row per clock / stride cols per clock 
            Number of output windows per row per clock must be integer or 
            reciprocal of one so that the position of output windows relative
            to the new pixels each clock is constant. 

            Otherwise there will be different numbers of new pixels in
            each window in each clock. This is challenging to implement.
            """
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: stride_cols {} not divisible by"
                            "pixels_per_row_per_clock {} nor vice-verse. One of them must"
                            "be divisible by the other. \n Reason: {}"
                            .format(stride_cols, pixels_per_row_per_clock, reason))

        if (stride_rows % rows_of_pixels_per_clock != 0 and
                rows_of_pixels_per_clock % stride_rows != 0):
            reason = """
            the average number of rows of output windows per clock =
                rows of pixels per clock / stride rows per clock 
            Number of rows of output windows per clock must be integer or 
            reciprocal of one so that the position of output windows relative
            to the new pixels each clock is constant. 

            Otherwise there will be different numbers of new pixels in
            each window in each clock. This is challenging to implement.
            """
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: stride_rows {} not divisible by"
                            "rows_of_pixels_per_clock {} nor vice-verse. One of them must"
                            "be divisible by the other. \n Reason: {}"
                            .format(stride_rows, rows_of_pixels_per_clock, reason))

        if ((stride_cols * stride_rows) %
            (pixels_per_row_per_clock * rows_of_pixels_per_clock) != 0) and \
                ((pixels_per_row_per_clock * rows_of_pixels_per_clock) %
                 (stride_cols * stride_rows) != 0):
            reason = """
            the average number of output windows per clock =
                (pixels per row per clock * rows of pixels per clock) /
                (stride cols per clock * stride rows per clock)
            Number of output windows per clock must be integer or 
            reciprocal of one so that throughput is an easier factor to manipulate 
            with map/underutil.
            
            Otherwise throughput is a weird fraction and the downstream system is
            either only partially used on some clocks or the sequence length
            is multiplied by a weird factor that makes the rational number
            throughput become an integer.
            """
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: stride_cols {} * stride_rows{} not divisible by"
                            "pixels_per_row_per_clock {} * rows_of_pixels_per_clock nor vice-verse. One of them must"
                            "be divisible by the other. \n Reason: {}"
                            .format(stride_cols, stride_rows, pixels_per_row_per_clock,
                                    rows_of_pixels_per_clock, reason))

        if abs(origin_cols) >= window_cols:
            reason = """
            origin must be less than window. If abs(origin_cols) was greater
            than window_cols, then entire first window would be garbage         
            """
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: |origin| {} greater than or equal to"
                            "window width {}".format(abs(origin_cols),
                                                     window_cols))

        if abs(origin_rows) >= window_rows:
            reason = """
            origin must be less than window. If abs(origin_rows) was greater
            than window_rows, then entire first window would be garbage         
            """
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: |origin| {} greater than or equal to"
                            "window width {}".format(abs(origin_rows),
                                                     window_rows))
        if origin_cols > 0:
            reason = """
            origin_cols can't go into image. That would be cropping the first cols of the image
            and linebuffer doesn't do cropping.
            """
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: origin_cols {} greater than 0. \n Reason: {}"
                            .format(origin_cols, reason))

        if origin_rows > 0:
            reason = """
            origin_rows can't go into image. That would be cropping the first rows of the image
            and linebuffer doesn't do cropping.
            """
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: origin_rows {} greater than 0. \n Reason: {}"
                            .format(origin_rows, reason))

        if window_cols - origin_cols >= image_cols:
            reason = """
            need window_cols plus abs(origin_cols) outputs to do wiring.
            If the image_cols is smaller than this, will have issues with
            internal wiring. Additionally, the linebuffer isn't
            used for images that are small enough to be processed
            in one or a few clock cycles. This is a weird edge 
            case that I don't want to deal with and shouldn't occur
            in the real world.
            """
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: window_cols {} - origin_cols {} "
                            "greater than or equal to image_cols {}. \n Reason: {}"
                            .format(window_cols, origin_cols, image_cols, reason))

        if window_rows - origin_rows >= image_rows:
            reason = """
            need window_rows plus abs(origin_rows) outputs to do wiring.
            If the image_rows is smaller than this, will have issues with
            internal wiring. Additionally, the linebuffer isn't
            used for images that are small enough to be processed
            in one or a few clock cycles. This is a weird edge 
            case that I don't want to deal with and shouldn't occur
            in the real world.
            """
            raise Exception("Aetherling's Native LineBuffer has invalid "
                            "parameters: window_rows {} - origin_rows {} "
                            "greater than or equal to image_rows {}. \n Reason: {}"
                            .format(window_rows, origin_rows, image_rows, reason))

        name = "TwoDimensionalLineBuffer_{}type_{}x{}pxPerClock_{}x{}window" \
               "_{}x{}img_{}x{}stride_{}x{}origin".format(
            cleanName(str(pixel_type)),
            pixels_per_row_per_clock,
            rows_of_pixels_per_clock,
            window_cols,
            window_rows,
            image_cols,
            image_rows,
            stride_cols,
            stride_rows,
            origin_cols,
            origin_rows
        )

        # if pixel_per_clock greater than stride, emitting that many new windows per clock
        # else just emit one per clock when have enough pixels to do so
        windows_per_row_per_clock = max(pixels_per_row_per_clock // stride_cols, 1)
        rows_of_windows_per_clock = max(rows_of_pixels_per_clock // stride_rows, 1)
        # if stride in y (stride_rows) is greater than parallelism across rows,
        # then need a buffer, so windows_per_active_clock is for average rate of windows out over time
        if stride_rows <= rows_of_pixels_per_clock:
            windows_per_active_clock = windows_per_row_per_clock * rows_of_windows_per_clock
        else:
            # average pixels per clock out = (note: if less than 1 window out every clock, then 1 is floor
            # as will emit 1 sometimes)

            # this division is integer as condition already holds that stride_rows > rows_of_pixels_per_clock
            # and image_cols must be divisible by parallelism across columns

            time_per_buffered_cycle = ((image_cols * stride_rows) //
                                       (rows_of_pixels_per_clock * pixels_per_row_per_clock))
            windows_per_active_clock = max(
                # input pixels (pixels in first row not dropped) divided by time to complete window
                (image_cols // stride_cols) // time_per_buffered_cycle, 1
            )

        IO = ['I', In(Array(rows_of_pixels_per_clock, Array(pixels_per_row_per_clock, In(pixel_type)))),
              'O', Out(Array(windows_per_active_clock, Array(window_rows, Array(window_cols, Out(pixel_type))))),
              'valid', Out(Bit), 'ready', Out(Bit)] + ClockInterface(has_ce=True)
        @classmethod
        def definition(cls):
            lb = AnyDimensionalLineBuffer(
                cirb,
                pixel_type,
                [rows_of_pixels_per_clock, pixels_per_row_per_clock],
                [window_rows, window_cols],
                [image_rows, image_cols],
                [stride_rows, stride_cols],
                [origin_rows, origin_cols]
            )
            wire(cls.I, lb.I)
            if stride_rows <= rows_of_pixels_per_clock:
                for row_of_windows in range(cls.rows_of_windows_per_clock):
                    for window_per_row in range(cls.windows_per_row_per_clock):
                        wire(cls.O[row_of_windows * cls.windows_per_row_per_clock + window_per_row],
                             lb.O[row_of_windows][window_per_row])
                wire(cls.valid, lb.valid)

            else:
                db = DelayedBuffer(cirb,  Array(window_rows, Array(window_cols, pixel_type)), image_cols // stride_cols,
                                   max(pixels_per_row_per_clock // stride_cols, 1),
                                   cls.time_per_buffered_cycle)
                for row_of_windows in range(cls.rows_of_windows_per_clock):
                    for window_per_row in range(cls.windows_per_row_per_clock):
                        wire(db.I[row_of_windows * cls.windows_per_row_per_clock + window_per_row],
                             lb.O[row_of_windows][window_per_row])
                wire(lb.valid, db.WE)
                wire(db.valid, cls.valid)
                wire(db.O, cls.O)

                # first time lb is valid, delayed buffer becomes
                # valid permanently
                first_valid_counter = SizedCounterModM(2, has_ce=True)
                zero_const = DefineCoreirConst(1, 0)()
                wire(lb.valid & (zero_const.O == first_valid_counter.O),
                     first_valid_counter.CE)
                # need lb.valid or counter as lb.valid will be 1 on first clock where valid
                # while counter will still be 0
                wire((lb.valid | first_valid_counter.O[0]) & bit(cls.CE), db.CE)

            wire(cls.CE, lb.CE)
            wire(cls.ready, 1)

    return _LB

def TwoDimensionalLineBuffer(
        cirb: CoreIRBackend,
        pixel_type: Kind,
        pixels_per_row_per_clock: int,
        rows_of_pixels_per_clock: int,
        window_cols: int,
        window_rows: int,
        image_cols: int,
        image_rows: int,
        stride_cols: int,
        stride_rows: int,
        origin_cols: int,
        origin_rows: int) -> Circuit:
    """
    :param cirb: The CoreIR backend currently be used
    :param pixel_type: the type of each pixel. A type of Array(3, Array(8, Bit)) is for
    3 color channel, 8 bits per channel.
    :param pixels_per_row_per_clock: The number of pixels per row of the image the linebuffer
    receives as input each clock.
    :param rows_of_pixels_per_clock: The number of rows of pixels of the image the linebuffer
    receives as input each clock.
    :param window_cols: The number of columns in the stencils that are emitted
    :param window_rows: The number of rows in the stencils that are emitted
    :param image_cols: The number of columns in the 2D input image
    :param image_rows: The number of rows in the 2D input image
    :param stride_cols: The distance between origins of consecutive stencils in number of pixels
    in the columns dimension. A stride of 1 means that successive stencils have origins in adjacent
    columns.
    :param stride_rows: The distance between origins of consecutive stencils in number of pixels
    in the rows dimension. A stride of 1 means that successive stencils have origins in adjacent
    rows.
    :param origin_cols: The column of the first pixel of the first window relative to
    the top left corner of the image
    :param origin_rows: The row of the first pixel of the first window relative to
    the top left corner of the image

    Restrictions:

    1. image_cols % pixels_per_row_per_clock == 0
    2. image_rows % rows_of_pixels_per_clock == 0
    3. if rows_of_pixels_per_clock > 1, then image_cols == pixels_per_row_per_clock
    4. image_cols % stride_cols == 0
    5. image_rows % stride_rows == 0
    6. (stride_cols * stride_rows) %
         (pixels_per_row_per_clock * rows_of_pixels_per_clock) == 0 OR
       (pixels_per_row_per_clock * rows_of_pixels_per_clock) %
         (stride_cols * stride_rows) == 0
    7. window_cols > |origin_cols|
    8. window_rows > |origin_rows|
    9. origin_cols <= 0
    10. origin_rows <= 0
    11. window_cols - origin_cols < image_cols
    12. window_rows - origin_rows < image_rows

    :return: A 2D Linebuffer with ports I, O, valid, CE, and next_row (if last_row false)
    """
    DefineTwoDimensionalLineBuffer(
        cirb,
        pixel_type,
        pixels_per_row_per_clock,
        rows_of_pixels_per_clock,
        window_cols,
        window_rows,
        image_cols,
        image_rows,
        stride_cols,
        stride_rows,
        origin_cols,
        origin_rows
    )()
