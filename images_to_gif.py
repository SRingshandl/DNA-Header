import os
from PIL import Image

folder_path = r".\Output"
print("Reading files from this folder: " + str(folder_path))
print("Obtained files: " + str(len(os.listdir(folder_path))))

image_list = []
for file in os.listdir(folder_path):
    if file.endswith(".png"):
        image_list.append(os.path.join(folder_path, file))
print("of which valid images: " + str(len(image_list)))

additional_image_list = []
for i in range(1, len(image_list)):
    additional_image_list.append(Image.open(image_list[i]))

startimage = Image.open(image_list[0])
startimage.save("generated_output.gif", save_all=True, append_images=additional_image_list, duration=20, loop=0)

print("Job completed and output generated")
