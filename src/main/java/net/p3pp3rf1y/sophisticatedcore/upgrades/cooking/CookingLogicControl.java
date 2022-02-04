package net.p3pp3rf1y.sophisticatedcore.upgrades.cooking;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.crafting.AbstractCookingRecipe;
import net.minecraft.world.level.Level;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.CompositeWidgetBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ProgressBar;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.WidgetBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.UV;

import java.util.List;

import static net.p3pp3rf1y.sophisticatedcore.client.gui.utils.GuiHelper.GUI_CONTROLS;

public class CookingLogicControl<T extends AbstractCookingRecipe> extends CompositeWidgetBase<WidgetBase> {
	private static final TextureBlitData FURNACE_BACKGROUND = new TextureBlitData(GUI_CONTROLS, Dimension.SQUARE_256, new UV(29, 202), new Dimension(68, 54));
	private static final TextureBlitData COOK_PROGRESS = new TextureBlitData(GUI_CONTROLS, Dimension.SQUARE_256, new UV(100, 239), new Dimension(22, 16));
	private static final TextureBlitData BURN_PROGRESS = new TextureBlitData(GUI_CONTROLS, Dimension.SQUARE_256, new UV(99, 225), new Dimension(14, 14));
	private final CookingLogicContainer<T> cookingLogicContainer;

	public CookingLogicControl(Position position, CookingLogicContainer<T> cookingLogicContainer) {
		super(position, new Dimension(68, 54));
		this.cookingLogicContainer = cookingLogicContainer;
		addChild(new ProgressBar(new Position(x + 19, y + 18), COOK_PROGRESS, this::getCookProgress, ProgressBar.ProgressDirection.LEFT_RIGHT));
		addChild(new ProgressBar(new Position(x + 1, y + 20), BURN_PROGRESS, this::getBurnProgress, ProgressBar.ProgressDirection.BOTTOM_UP));
	}

	private float getBurnProgress() {
		//noinspection ConstantConditions - world is not null by this point
		return cookingLogicContainer.isBurning(Minecraft.getInstance().level) ? getProgress(cookingLogicContainer.getBurnTimeFinish(), cookingLogicContainer.getBurnTimeTotal()) : 0;
	}

	private float getCookProgress() {
		return cookingLogicContainer.isCooking() ? getProgress(cookingLogicContainer.getCookTimeFinish(), cookingLogicContainer.getCookTimeTotal()) : 0;
	}

	private float getProgress(long finishTime, int timeTotal) {
		Level world = Minecraft.getInstance().level;
		if (world == null) {
			return 0;
		}
		return 1 - ((float) Math.max(finishTime - world.getGameTime(), 0) / timeTotal);
	}

	@Override
	protected void renderBg(PoseStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		GuiHelper.blit(matrixStack, x, y, FURNACE_BACKGROUND);
	}

	public void moveSlotsToView(int screenGuiLeft, int screenGuiTop) {
		List<Slot> smeltingSlots = cookingLogicContainer.getCookingSlots();
		positionSlot(smeltingSlots.get(CookingLogic.COOK_INPUT_SLOT), screenGuiLeft, screenGuiTop, 1, 1);
		positionSlot(smeltingSlots.get(CookingLogic.COOK_OUTPUT_SLOT), screenGuiLeft, screenGuiTop, 47, 19);
		positionSlot(smeltingSlots.get(CookingLogic.FUEL_SLOT), screenGuiLeft, screenGuiTop, 1, 37);
	}

	private void positionSlot(Slot slot, int screenGuiLeft, int screenGuiTop, int xOffset, int yOffset) {
		slot.x = x - screenGuiLeft + xOffset;
		slot.y = y - screenGuiTop + yOffset;
	}

	@Override
	public void updateNarration(NarrationElementOutput pNarrationElementOutput) {
		//TODO add narration
	}
}
