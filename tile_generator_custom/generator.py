import numpy as np
from PIL import Image

# Configuration
INPUT_IMAGE = 'tile_generator_custom/here.png'
OUTPUT_MEM = 'tile_generator_custom/image_data.mem'
ROWS = 32
COLS = 32

print("... Loading input PNG image")
try:
    # Open the image and ensure it's in RGBA mode
    img = Image.open(INPUT_IMAGE).convert('RGBA')
except FileNotFoundError:
    print(f"Error: Could not find the file at {INPUT_IMAGE}")
    exit()

# Verify dimensions
if img.size != (COLS, ROWS):
    print(f"Warning: Image size is {img.size}, resizing to {COLS}x{ROWS}")
    img = img.resize((COLS, ROWS))

# Convert to a NumPy array of shape (32, 32, 4)
img_array = np.array(img)

# Extract individual channels
# Scale 0-255 down to 2 bits (0-3) by integer dividing by 64
r_2bit = img_array[:, :, 0] // 64
g_2bit = img_array[:, :, 1] // 64
b_2bit = img_array[:, :, 2] // 64

# Alpha channel: standard behavior is 0 = fully transparent, 255 = fully opaque.
# For your 1-bit transparency, let's assume: Transparent = 1, Opaque = 0.
# (If you want Opaque = 1 and Transparent = 0, swap the 1 and 0 in the np.where)
a_1bit = np.where(img_array[:, :, 3] < 128, 1, 0)

print("... Mapping pixels to 7-bit format")
# Shift bits into their respective positions:
# A is at bit 6, R at bits 5-4, G at bits 3-2, B at bits 1-0
custom_7bit_array = (a_1bit << 6) | (r_2bit << 4) | (g_2bit << 2) | b_2bit

# Flatten the 2D array into a 1D array of 1024 pixels
flattened_data = custom_7bit_array.flatten()

print(f"... Saving to memory file: {OUTPUT_MEM}")
# Write to a .mem file (text format containing hexadecimal strings)
with open(OUTPUT_MEM, 'w') as f:
    for pixel in flattened_data:
        if pixel >> 6 & 1:
            pixel = 0b1000000
        # Formats the byte as a 2-digit uppercase hex string (e.g., "7F")
        f.write(f"{pixel:07b}\n")

print("... File successfully saved!")