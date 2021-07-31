package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.stonecutter;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.audio.SimpleSound;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.StonecuttingRecipe;
import net.minecraft.util.SoundEvents;
import net.minecraft.util.math.MathHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.Widget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;

import java.util.List;

public class StonecutterRecipeControl extends Widget {
	private static final TextureBlitData SLIDER = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(29, 131), Dimension.RECTANGLE_12_15);
	private static final TextureBlitData DISABLED_SLIDER = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(41, 131), Dimension.RECTANGLE_12_15);
	private static final TextureBlitData RECIPE_BACKGROUND = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(110, 148), Dimension.RECTANGLE_16_18);
	private static final TextureBlitData SELECTED_RECIPE_BACKGROUND = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(110, 166), Dimension.RECTANGLE_16_18);
	private static final TextureBlitData RECIPE_BACKGROUND_HOVERED = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(110, 184), Dimension.RECTANGLE_16_18);
	private static final TextureBlitData LIST_BACKGROUND = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(29, 146), new Dimension(81, 56));

	private static final int LIST_Y_OFFSET = 22;
	private static final int INPUT_SLOT_HEIGHT = 18;
	private static final int SPACING = 4;

	private boolean clickedOnScroll;
	private final BackpackScreen screen;
	private final StonecutterRecipeContainer container;
	private boolean hasItemsInInputSlot;
	private int recipeIndexOffset;
	private float sliderProgress;

	protected StonecutterRecipeControl(BackpackScreen screen, StonecutterRecipeContainer container, Position position) {
		super(position);
		this.screen = screen;
		this.container = container;
		container.setInventoryUpdateListener(this::onInventoryUpdate);
		onInventoryUpdate();
	}

	public void moveSlotsToView() {
		Slot inputSlot = container.getInputSlot();
		inputSlot.x = x + getCenteredX(16) - screen.getGuiLeft();
		inputSlot.y = y - screen.getGuiTop() + 1;
		Slot outputSlot = container.getOutputSlot();
		outputSlot.x = x + getCenteredX(16) - screen.getGuiLeft();
		outputSlot.y = inputSlot.y + INPUT_SLOT_HEIGHT + SPACING + LIST_BACKGROUND.getHeight() + SPACING + 4;
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		GuiHelper.renderSlotsBackground(minecraft, matrixStack, x + getCenteredX(18), y, 1, 1);
		GuiHelper.blit(minecraft, matrixStack, x, y + LIST_Y_OFFSET, LIST_BACKGROUND);
		GuiHelper.blit(minecraft, matrixStack, x + getCenteredX(26), y + INPUT_SLOT_HEIGHT + SPACING + LIST_BACKGROUND.getHeight() + SPACING, GuiHelper.CRAFTING_RESULT_SLOT);
		int sliderYOffset = (int) (39.0F * sliderProgress) + 1;
		GuiHelper.blit(minecraft, matrixStack, x + 68, y + LIST_Y_OFFSET + sliderYOffset, canScroll() ? SLIDER : DISABLED_SLIDER);

		int listInnerLeftX = x + 1;
		int listTopY = getListTopY();
		int recipeIndexOffsetMax = recipeIndexOffset + 12;
		renderRecipeBackgrounds(matrixStack, mouseX, mouseY, listInnerLeftX, listTopY, recipeIndexOffsetMax);
		drawRecipesItems(matrixStack, listInnerLeftX, listTopY, recipeIndexOffsetMax);
	}

	private void drawRecipesItems(MatrixStack matrixStack, int listInnerLeftX, int top, int recipeIndexOffsetMax) {
		List<StonecuttingRecipe> list = container.getRecipeList();

		for (int i = recipeIndexOffset; i < recipeIndexOffsetMax && i < container.getRecipeList().size(); ++i) {
			int j = i - recipeIndexOffset;
			int k = listInnerLeftX + j % 4 * 16;
			int l = j / 4;
			int i1 = top + l * 18 + 2;
			GuiHelper.renderItemInGUI(matrixStack, minecraft, list.get(i).getResultItem(), k, i1);
		}

	}

	private int getListTopY() {
		return y + LIST_Y_OFFSET;
	}

	private void renderRecipeBackgrounds(MatrixStack matrixStack, int mouseX, int mouseY, int listInnerLeftX, int listTopY, int recipeIndexOffsetMax) {
		for (int recipeIndex = recipeIndexOffset; recipeIndex < recipeIndexOffsetMax && recipeIndex < container.getRecipeList().size(); ++recipeIndex) {
			int j = recipeIndex - recipeIndexOffset;
			int recipeX = listInnerLeftX + j % 4 * 16;
			int row = j / 4;
			int recipeY = listTopY + row * 18 + 2;
			TextureBlitData background = RECIPE_BACKGROUND;

			if (recipeIndex == container.getSelectedRecipe()) {
				background = SELECTED_RECIPE_BACKGROUND;
			} else if (mouseX >= recipeX && mouseY >= recipeY && mouseX < recipeX + 16 && mouseY < recipeY + 18) {
				background = RECIPE_BACKGROUND_HOVERED;
			}

			GuiHelper.blit(minecraft, matrixStack, recipeX, recipeY - 1, background);
		}
	}

	private boolean canScroll() {
		return hasItemsInInputSlot && container.getRecipeList().size() > 12;
	}

	@Override
	protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		//noop - everything is rendered in background or after screen render is done
	}

	@Override
	public void afterScreenRender(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		super.afterScreenRender(matrixStack, mouseX, mouseY, partialTicks);
		renderHoveredTooltip(matrixStack, mouseX, mouseY);
	}

	private void renderHoveredTooltip(MatrixStack matrixStack, int mouseX, int mouseY) {
		if (hasItemsInInputSlot) {
			int listTopY = getListTopY();
			int k = recipeIndexOffset + 12;
			List<StonecuttingRecipe> list = container.getRecipeList();

			for (int recipeIndex = recipeIndexOffset; recipeIndex < k && recipeIndex < list.size(); ++recipeIndex) {
				int inviewRecipeIndex = recipeIndex - recipeIndexOffset;
				int recipeLeftX = x + inviewRecipeIndex % 4 * 16;
				int k1 = listTopY + inviewRecipeIndex / 4 * 18 + 2;
				if (mouseX >= recipeLeftX && mouseX < recipeLeftX + 16 && mouseY >= k1 && mouseY < k1 + 18) {
					renderTooltip(matrixStack, list.get(recipeIndex).getResultItem(), mouseX, mouseY);
				}
			}
		}
	}

	private void renderTooltip(MatrixStack matrixStack, ItemStack itemStack, int mouseX, int mouseY) {
		FontRenderer font = itemStack.getItem().getFontRenderer(itemStack);
		net.minecraftforge.fml.client.gui.GuiUtils.preItemToolTip(itemStack);
		screen.renderWrappedToolTip(matrixStack, screen.getTooltipFromItem(itemStack), mouseX, mouseY, (font == null ? this.font : font));
		net.minecraftforge.fml.client.gui.GuiUtils.postItemToolTip();
	}

	private void onInventoryUpdate() {
		hasItemsInInputSlot = container.hasItemsInInputSlot();
		if (!hasItemsInInputSlot) {
			sliderProgress = 0.0F;
			recipeIndexOffset = 0;
		} else if (container.getSelectedRecipe() - recipeIndexOffset >= 12) {
			int rowsToScroll = (container.getSelectedRecipe() - recipeIndexOffset - 12) / 4 + 1;
			scrollRecipesByDelta(-rowsToScroll);
		}
	}

	@Override
	public boolean mouseClicked(double mouseX, double mouseY, int button) {
		clickedOnScroll = false;
		if (hasItemsInInputSlot) {
			int listInnerLeftX = x + 1;
			int listInnerTopY = y + LIST_Y_OFFSET + 1;
			int maxRecipeIndexOffset = recipeIndexOffset + 12;

			for (int recipeIndex = recipeIndexOffset; recipeIndex < maxRecipeIndexOffset; ++recipeIndex) {
				int visibleRecipeIndex = recipeIndex - recipeIndexOffset;
				double relativeX = mouseX - (double) (listInnerLeftX + visibleRecipeIndex % 4 * 16);
				double relativeY = mouseY - (double) (listInnerTopY + Math.floorDiv(visibleRecipeIndex, 4) * 18);
				if (relativeX >= 0.0D && relativeY >= 0.0D && relativeX < 16.0D && relativeY < 18.0D && container.selectRecipe(recipeIndex)) {
					Minecraft.getInstance().getSoundManager().play(SimpleSound.forUI(SoundEvents.UI_STONECUTTER_SELECT_RECIPE, 1.0F));
					return true;
				}
			}

			int sliderLeftX = listInnerLeftX + 67;
			if (mouseX >= (double) sliderLeftX && mouseX < (double) (sliderLeftX + 12) && mouseY >= (double) listInnerTopY && mouseY < (double) (listInnerTopY + 54)) {
				clickedOnScroll = true;
				return true;
			}
		}

		return super.mouseClicked(mouseX, mouseY, button);
	}

	@Override
	public boolean mouseDragged(double mouseX, double mouseY, int button, double dragX, double dragY) {
		if (clickedOnScroll && canScroll()) {
			int listTopY = y + LIST_Y_OFFSET;
			int listBottomY = listTopY + 54;
			sliderProgress = ((float) mouseY - (float) listTopY - 7.5F) / ((float) (listBottomY - listTopY) - 15.0F);
			sliderProgress = MathHelper.clamp(sliderProgress, 0.0F, 1.0F);
			recipeIndexOffset = (int) ((double) (sliderProgress * (float) getHiddenRows()) + 0.5D) * 4;
			return true;
		} else {
			return super.mouseDragged(mouseX, mouseY, button, dragX, dragY);
		}
	}

	@Override
	public boolean mouseScrolled(double mouseX, double mouseY, double delta) {
		if (canScroll()) {
			scrollRecipesByDelta(delta);
		}
		return true;
	}

	private void scrollRecipesByDelta(double delta) {
		int i = getHiddenRows();
		sliderProgress = (float) ((double) sliderProgress - delta / (double) i);
		sliderProgress = MathHelper.clamp(sliderProgress, 0.0F, 1.0F);
		recipeIndexOffset = (int) ((double) (sliderProgress * (float) i) + 0.5D) * 4;
	}

	protected int getHiddenRows() {
		return (container.getRecipeList().size() + 4 - 1) / 4 - 3;
	}

	@Override
	public int getWidth() {
		return 81;
	}

	@Override
	public int getHeight() {
		return 108;
	}
}
