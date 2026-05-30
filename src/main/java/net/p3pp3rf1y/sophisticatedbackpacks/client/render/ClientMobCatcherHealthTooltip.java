package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.screens.inventory.tooltip.ClientTooltipComponent;
import net.minecraft.resources.ResourceLocation;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher.MobCatcherHealthTooltip;

public class ClientMobCatcherHealthTooltip implements ClientTooltipComponent {
	private static final ResourceLocation EMPTY_HEART = ResourceLocation.withDefaultNamespace("hud/heart/container");
	private static final ResourceLocation FULL_HEART = ResourceLocation.withDefaultNamespace("hud/heart/full");
	private static final ResourceLocation HALF_HEART = ResourceLocation.withDefaultNamespace("hud/heart/half");
	private static final int HEART_SIZE = 9;
	private static final int MAX_INLINE_HEARTS = 10;

	private final int currentHealth;
	private final int maxHealth;

	public ClientMobCatcherHealthTooltip(MobCatcherHealthTooltip tooltip) {
		maxHealth = Math.max(1, tooltip.maxHealth());
		currentHealth = Math.min(maxHealth, Math.max(0, tooltip.currentHealth()));
	}

	@Override
	public int getHeight() {
		return HEART_SIZE + 1;
	}

	@Override
	public int getWidth(Font font) {
		return isCompact() ? getCompactWidth(font) : getInlineHeartCount() * HEART_SIZE;
	}

	@Override
	public void renderImage(Font font, int x, int y, GuiGraphics guiGraphics) {
		if (isCompact()) {
			renderCompact(font, guiGraphics, x, y);
		} else {
			renderInline(guiGraphics, x, y);
		}
	}

	private void renderInline(GuiGraphics guiGraphics, int x, int y) {
		for (int heart = 0; heart < getInlineHeartCount(); heart++) {
			renderHeart(guiGraphics, x + heart * HEART_SIZE, y, EMPTY_HEART);
			int remainingHealth = currentHealth - heart * 2;
			if (remainingHealth >= 2) {
				renderHeart(guiGraphics, x + heart * HEART_SIZE, y, FULL_HEART);
			} else if (remainingHealth == 1) {
				renderHeart(guiGraphics, x + heart * HEART_SIZE, y, HALF_HEART);
			}
		}
	}

	private void renderCompact(Font font, GuiGraphics guiGraphics, int x, int y) {
		renderCompactHearts(font, guiGraphics, x, y);
	}

	private boolean isCompact() {
		return maxHealth > MAX_INLINE_HEARTS * 2;
	}

	private int getInlineHeartCount() {
		return (maxHealth + 1) / 2;
	}

	private int getCompactWidth(Font font) {
		return getCompactHeartsWidth(font);
	}

	private void renderCompactHearts(Font font, GuiGraphics guiGraphics, int x, int y) {
		int fullHearts = currentHealth / 2;
		boolean hasHalfHeart = currentHealth % 2 == 1;
		int emptyHearts = (maxHealth + 1) / 2 - fullHearts - (hasHalfHeart ? 1 : 0);
		int nextX = x;
		if (fullHearts > 0) {
			String fullHeartCount = fullHearts + "x";
			guiGraphics.drawString(font, fullHeartCount, nextX, y + 1, 0xFF_FFFFFF, false);
			nextX += font.width(fullHeartCount) + 2;
			renderHeart(guiGraphics, nextX, y, FULL_HEART);
			nextX += HEART_SIZE + 3;
		}
		if (hasHalfHeart) {
			renderHeart(guiGraphics, nextX, y, EMPTY_HEART);
			renderHeart(guiGraphics, nextX, y, HALF_HEART);
			nextX += HEART_SIZE + 3;
		}
		if (emptyHearts > 0) {
			String emptyHeartCount = emptyHearts + "x";
			guiGraphics.drawString(font, emptyHeartCount, nextX, y + 1, 0xFF_AAAAAA, false);
			nextX += font.width(emptyHeartCount) + 2;
			renderHeart(guiGraphics, nextX, y, EMPTY_HEART);
		}
	}

	private int getCompactHeartsWidth(Font font) {
		int width = 0;
		int fullHearts = currentHealth / 2;
		boolean hasHalfHeart = currentHealth % 2 == 1;
		int emptyHearts = (maxHealth + 1) / 2 - fullHearts - (hasHalfHeart ? 1 : 0);
		if (fullHearts > 0) {
			width += font.width(fullHearts + "x") + 2 + HEART_SIZE + 3;
		}
		if (hasHalfHeart) {
			width += HEART_SIZE + 3;
		}
		if (emptyHearts > 0) {
			width += font.width(emptyHearts + "x") + 2 + HEART_SIZE;
		}
		return width;
	}

	private void renderHeart(GuiGraphics guiGraphics, int x, int y, ResourceLocation heartSprite) {
		guiGraphics.blitSprite(heartSprite, x, y, HEART_SIZE, HEART_SIZE);
	}
}
