package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.cooking;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.crafting.AbstractCookingRecipe;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.BackpackWidget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.CompositeBackpackWidget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ProgressBar;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;

import java.util.List;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper.GUI_CONTROLS;

public class CookingLogicControl<T extends AbstractCookingRecipe> extends CompositeBackpackWidget<BackpackWidget> {
	private static final TextureBlitData FURNACE_BACKGROUND = new TextureBlitData(GUI_CONTROLS, Dimension.SQUARE_256, new UV(29, 202), new Dimension(68, 54));
	private static final TextureBlitData COOK_PROGRESS = new TextureBlitData(GUI_CONTROLS, Dimension.SQUARE_256, new UV(100, 239), new Dimension(22, 16));
	private static final TextureBlitData BURN_PROGRESS = new TextureBlitData(GUI_CONTROLS, Dimension.SQUARE_256, new UV(99, 225), new Dimension(14, 14));
	private final CookingLogicContainer<T> cookingLogicContainer;

	public CookingLogicControl(Position position, CookingLogicContainer<T> cookingLogicContainer) {
		super(position);
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
		World world = Minecraft.getInstance().level;
		if (world == null) {
			return 0;
		}
		return 1 - ((float) Math.max(finishTime - world.getGameTime(), 0) / timeTotal);
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		GuiHelper.blit(minecraft, matrixStack, x, y, FURNACE_BACKGROUND);
	}

	@Override
	public int getWidth() {
		return 68;
	}

	@Override
	public int getHeight() {
		return 54;
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
}
